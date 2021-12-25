open! Core
open! Async
module P = Postgresql

module Result_status = struct
  type t = P.result_status =
    | Empty_query (** String sent to the backend was empty *)
    | Command_ok (** Successful completion of a command returning no data *)
    | Tuples_ok (** The query successfully executed *)
    | Copy_out (** Copy Out (from server) data transfer started *)
    | Copy_in (** Copy In (to server) data transfer started *)
    | Bad_response (** The server's response was not understood *)
    | Nonfatal_error
    | Fatal_error
    | Copy_both
    | Single_tuple (** One tuple of a result set ({!set_single_row_mode}) *)
  [@@deriving sexp]

  let is_ok f = function
    | Single_tuple | Tuples_ok | Command_ok -> Ok ()
    | other -> Error (Error.create_s [%sexp (other : t), (f () : string)])
  ;;
end

module Result = struct
  module Status = Result_status

  type t = P.result

  let status (t : t) = t#status
  let is_ok (t : t) = Result_status.is_ok (fun () -> t#error) t#status
  let nfields (t : t) = t#nfields
  let ntuples (t : t) = t#ntuples
  let get_value (t : t) i k = t#getvalue i k
  let get_escaped_value (t : t) i k = t#get_escaped_value i k
  let fname (t : t) i = t#fname i

  let sexp_of_t t =
    let fields = List.init (nfields t) ~f:(fun i -> Sexp.Atom (fname t i)) in
    ntuples t
    |> List.init ~f:(fun tuple ->
           fields
           |> List.mapi ~f:(fun i name ->
                  Sexp.List [ name; Sexp.Atom (get_value t tuple i) ])
           |> Sexp.List)
    |> Sexp.List
  ;;
end

type t =
  { conn : P.connection
  ; fd : Async.Fd.t
  }

let underlying { conn; _ } = conn
let fd { fd; _ } = fd

let make_fd i =
  Async.Fd.create
    (Socket `Active)
    (Core.Unix.File_descr.of_int i)
    (Info.of_string "pgsql connection")
;;

let status_is_ok { conn; _ } =
  match conn#status with
  | Bad -> Deferred.return (Error (Error.of_string conn#error_message))
  | Connection_started
  | Connection_made
  | Connection_awaiting_response
  | Connection_auth_ok
  | Connection_setenv
  | Connection_ssl_startup
  | Ok -> return (Ok ())
;;

let wait_for_result { conn; fd } =
  Deferred.Or_error.try_with (fun () ->
      conn#consume_input;
      while%bind return conn#is_busy do
        (* apparently there's an Async.Fd function that will call you back 
           if there's reads available on the descriptor. *)
        match%map Async.Fd.ready_to fd `Read with
        | `Ready -> conn#consume_input
        | `Closed -> Error.raise (Error.of_string "fd closed")
        | `Bad_fd -> Error.raise (Error.of_string "bad fd")
      done)
;;

let fetch_result ?(stop = Deferred.never ()) ({ conn; _ } as t) =
  let open Deferred.Or_error.Let_syntax in
  Async.upon stop (fun () -> conn#request_cancel);
  let%bind () = wait_for_result t in
  let%bind () = status_is_ok t in
  match%bind Deferred.return (Or_error.try_with (fun () -> conn#get_result)) with
  | None -> return None
  | Some r ->
    (match Result.is_ok r with
    | Ok () -> return (Some r)
    | Error e -> Deferred.return (Error e))
;;

let fetch_iter t ~f =
  let open Deferred.Or_error.Let_syntax in
  t.conn#set_single_row_mode;
  Deferred.Or_error.repeat_until_finished () (fun () ->
      match%bind fetch_result t with
      | None -> return (`Finished ())
      | Some r ->
        (match Result.status r with
        | Result_status.Tuples_ok -> return (`Finished ())
        | _ ->
          (match%bind f r with
          | `Stop ->
            t.conn#request_cancel;
            let%bind () =
              Deferred.Or_error.try_with (fun () ->
                  let open Deferred.Let_syntax in
                  while%bind
                    match%map fetch_result t with
                    | Error e -> Error.raise e
                    | Ok None -> true
                    | Ok (Some res) ->
                      print_s [%message (Result.status res : Result_status.t)];
                      false
                  do
                    return ()
                  done)
            in
            return (`Finished ())
          | `Continue -> return (`Repeat ()))))
;;

let fetch_single_result ?stop t =
  match%bind fetch_result ?stop t with
  | Error e -> return (Error e)
  | Ok None ->
    return (Error (Error.of_string "no results returned from fetch_single_result"))
  | Ok (Some r) ->
    (match%map fetch_result t with
    | Error e -> Error e
    | Ok None -> Ok r
    | Ok (Some _) ->
      Error (Error.of_string "more than one result returned from fetch_single_result"))
;;

let rec finish_conn ~fd f = function
  | P.Polling_failed -> return (Error (Error.of_string "polling failed"))
  | Polling_reading ->
    let%bind (_ : _) = Async.Fd.ready_to fd `Read in
    finish_conn ~fd f (f ())
  | Polling_writing ->
    let%bind (_ : _) = Async.Fd.ready_to fd `Write in
    finish_conn ~fd f (f ())
  | Polling_ok -> return (Ok ())
;;

let create ~host ~user ~port =
  let open Deferred.Or_error.Let_syntax in
  let conninfo = sprintf "host=%s user=%s port=%d" host user port in
  let conn = new P.connection ~conninfo ~startonly:true () in
  let%bind () =
    let fd = make_fd conn#socket in
    let t = { conn; fd } in
    let%bind () =
      Deferred.Or_error.try_with_join (fun () ->
          let%bind () = finish_conn ~fd (fun () -> conn#connect_poll) Polling_writing in
          status_is_ok t)
    in
    fd
    |> Async.Fd.close ~file_descriptor_handling:Do_not_close_file_descriptor
    |> Deferred.ok
  in
  conn#set_notice_processing `Quiet;
  conn#set_nonblocking true;
  assert conn#reset_start;
  let fd = make_fd conn#socket in
  let t = { conn; fd } in
  let%bind () =
    Deferred.Or_error.try_with_join (fun () ->
        let%bind () = finish_conn ~fd (fun () -> conn#reset_poll) Polling_writing in
        status_is_ok t)
  in
  return t
;;

let shutdown { conn; fd } =
  let%bind () =
    Async.Fd.close ~file_descriptor_handling:Do_not_close_file_descriptor fd
  in
  return (Or_error.try_with (fun () -> conn#finish))
;;
