open Core
open Async
open Postgresql

let failwith_f fmt = ksprintf failwith fmt

let with_fd (i : int) ~f =
  let fd =
    Async.Fd.create
      (Socket `Active)
      (Core.Unix.File_descr.of_int i)
      (Info.of_string "pgsql connection")
  in
  Async.Monitor.protect
    (fun () -> f fd)
    ~finally:(fun () ->
      Async.Fd.close ~file_descriptor_handling:Do_not_close_file_descriptor fd)
;;

let wait_for_result c ~fd =
  c#consume_input;
  while%bind return c#is_busy do
    (* apparently there's an Async.Fd function that will call you back 
    if there's reads available on the descriptor. *)
    let%map _a = Async.Fd.ready_to fd `Read in
    c#consume_input
  done
;;

let fetch_result c ~fd =
  let%map () = wait_for_result c ~fd in
  c#get_result
;;

let fetch_single_result c ~fd =
  match%bind fetch_result c ~fd with
  | None -> assert false
  | Some r ->
    (match%map fetch_result c ~fd with
    | None -> r
    | Some _ -> assert false)
;;

(* See http://www.postgresql.org/docs/devel/static/libpq-connect.html *)
let rec finish_conn ~fd connect_poll = function
  | Polling_failed ->
    printf "f\n%!";
    return ()
  | Polling_reading ->
    printf "r,%!";
    let%bind (_ : _) = Async.Fd.ready_to fd `Read in
    finish_conn ~fd connect_poll (connect_poll ())
  | Polling_writing ->
    printf "w,%!";
    let%bind (_ : _) = Async.Fd.ready_to fd `Write in
    finish_conn ~fd connect_poll (connect_poll ())
  | Polling_ok ->
    printf "c\n%!";
    return ()
;;

let test (c : connection) ~fd =
  (* Create a table using a non-prepared statement. *)
  c#send_query
    {| 
    CREATE TEMPORARY TABLE postgresql_ocaml_async (
      id SERIAL PRIMARY KEY, 
      a INTEGER NOT NULL, 
      b TEXT NOT NULL,
      c BYTEA
    ) |};
  let%bind () =
    let%map r = fetch_single_result c ~fd in
    match r#status with
    | Command_ok -> ()
    | _ -> assert false
  in
  (* Create another table which will trigger a notice. *)
  c#send_query
    {|
    CREATE TEMPORARY TABLE postgresql_ocaml_async_2 (
      id INTEGER PRIMARY KEY REFERENCES postgresql_ocaml_async ON DELETE CASCADE
    ) |};
  let%bind () =
    let%map r = fetch_single_result c ~fd in
    match r#status with
    | Command_ok -> ()
    | _ -> assert false
  in
  let%bind () =
    c#send_query
      ~param_types:Postgresql.[| oid_of_ftype INT8; oid_of_ftype INT8 |]
      ~params:[| "4100100100"; "5100100100" |]
      "SELECT $1 + $2";
    let%map r = fetch_single_result c ~fd in
    assert (Core.Poly.equal r#status Tuples_ok);
    assert (r#nfields = 1);
    assert (r#ntuples = 1);
    assert (String.equal (r#getvalue 0 0) "9200200200")
  in
  (* Populate using a prepared statement. *)
  let shown_ntuples = 10 in
  let expected_ntuples = 4 * 100 in
  let%bind () =
    Deferred.List.iter
      (List.range 0 3 ~stop:`inclusive)
      ~how:`Sequential
      ~f:(fun i ->
        let stmt = sprintf "test_ins_%d" i in
        let param_types =
          Array.sub
            Postgresql.[| oid_of_ftype INT4; oid_of_ftype TEXT; oid_of_ftype BYTEA |]
            ~pos:0
            ~len:i
        in
        c#send_prepare
          stmt
          ~param_types
          "INSERT INTO postgresql_ocaml_async (a, b, c) VALUES ($1, $2, $3)";
        let%bind () =
          let%map r = fetch_single_result c ~fd in
          assert (Poly.equal r#status Command_ok)
        in
        Deferred.List.iter
          (List.range 1 100 ~stop:`inclusive)
          ~how:`Sequential
          ~f:(fun j ->
            let c0 = string_of_int (i + (3 * j)) in
            let c1 = sprintf "# %d." (i + (3 * j)) in
            let c2 = c1 in
            c#send_query_prepared ~params:[| c0; c1; c2 |] stmt;
            let%map r = fetch_single_result c ~fd in
            assert (Poly.equal r#status Command_ok)))
  in
  (* Prepare a select statement. *)
  c#send_prepare "test_sel" "SELECT * FROM postgresql_ocaml_async";
  let%bind () =
    let%map r = fetch_single_result c ~fd in
    assert (Poly.equal r#status Command_ok)
  in
  (* Describe it. *)
  c#send_describe_prepared "test_sel";
  let%bind r = fetch_single_result c ~fd in
  assert (Poly.equal r#status Command_ok);
  assert (r#nfields = 4);
  assert (Poly.equal (r#fname 0) "id");
  assert (Poly.equal (r#fname 1) "a");
  assert (Poly.equal (r#fname 2) "b");
  (* Run it. *)
  c#send_query_prepared "test_sel";
  let%bind r = fetch_single_result c ~fd in
  assert (Poly.equal r#status Tuples_ok);
  print_s [%message (r#ntuples : int) (r#nfields : int)];
  assert (Poly.equal r#ntuples expected_ntuples);
  assert (r#nfields = 4);
  for i = 0 to min r#ntuples shown_ntuples - 1 do
    printf
      "%s, %s, %s, %s\n"
      (r#getvalue i 0)
      (r#getvalue i 1)
      (r#getvalue i 2)
      (r#get_escaped_value i 3)
  done;
  printf "[...]\n";
  (* Run it in single-row mode. *)
  c#send_query_prepared "test_sel";
  c#set_single_row_mode;
  let%bind () =
    Deferred.List.iter
      (List.range ~stop:`inclusive 0 expected_ntuples)
      ~how:`Sequential
      ~f:(fun i ->
        match%map fetch_result c ~fd with
        | None -> assert false
        | Some r when i < expected_ntuples ->
          assert (Poly.equal r#status Single_tuple);
          if i < shown_ntuples
          then printf "%s, %s, %s\n" (r#getvalue 0 0) (r#getvalue 0 1) (r#getvalue 0 2)
        | Some r -> assert (Poly.equal r#status Tuples_ok))
  in
  printf "[...]\n";
  let%bind () =
    match%map fetch_result c ~fd with
    | None -> ()
    | Some _ -> assert false
  in
  (* Drop the main table. *)
  c#send_query "DROP TABLE postgresql_ocaml_async CASCADE";
  let%bind () =
    let%map r = fetch_single_result c ~fd in
    assert (Poly.equal r#status Command_ok)
  in
  return ()
;;

let main conninfo =
  (* Async connect and test. *)
  let c = new connection ~conninfo ~startonly:true () in
  (* quiet notice processing is necessary; otherwise you get segfaults!
     https://github.com/mmottl/postgresql-ocaml/pull/38/files *)
  c#set_notice_processing `Quiet;
  let%bind () =
    with_fd c#socket ~f:(fun fd ->
        let%bind () = finish_conn ~fd (fun () -> c#connect_poll) Polling_writing in
        if Poly.equal c#status Bad then failwith_f "Connection failed: %s" c#error_message;
        assert (Poly.equal c#status Ok);
        c#set_nonblocking true;
        test c ~fd)
  in
  print_endline "NONBLOCKING";
  c#set_nonblocking true;
  assert c#reset_start;
  let%bind () =
    with_fd c#socket ~f:(fun fd ->
        let%bind () = finish_conn ~fd (fun () -> c#reset_poll) Polling_writing in
        if Poly.equal c#status Bad
        then failwith_f "Reset connection bad: %s" c#error_message;
        assert (Poly.equal c#status Ok);
        test c ~fd)
  in
  return ()
;;

let () =
  let flag, required_string = Command.Param.(flag, required string) in
  let command =
    let open Command.Let_syntax in
    let%map host = flag "-host" required_string ~doc:" hostname for database"
    and user = flag "-user" required_string ~doc:" database username" in
    let conninfo = sprintf "host=%s user=%s" host user in
    fun () -> main conninfo
  in
  Command.run (Command.async ~summary:"Connect to pgsql" command)
;;
