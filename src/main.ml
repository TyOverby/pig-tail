open Core
open Async
module P = Postgresql
module P_a = P_async
module Pa = P_async

let test t =
  let open Deferred.Or_error.Let_syntax in
  let c = Pa.underlying t in
  (* Create a table using a non-prepared statement. *)
  c#send_query
    {| 
    CREATE TEMPORARY TABLE postgresql_ocaml_async (
      id SERIAL PRIMARY KEY, 
      a INTEGER NOT NULL, 
      b TEXT NOT NULL,
      c BYTEA
    ) |};
  let%bind _ = Pa.fetch_single_result t in
  (* Create another table which will trigger a notice. *)
  c#send_query
    {|
    CREATE TEMPORARY TABLE postgresql_ocaml_async_2 (
      id INTEGER PRIMARY KEY REFERENCES postgresql_ocaml_async ON DELETE CASCADE
    ) |};
  let%bind _ = Pa.fetch_single_result t in
  let%bind () =
    c#send_query
      ~param_types:Postgresql.[| oid_of_ftype INT8; oid_of_ftype INT8 |]
      ~params:[| "4100100100"; "5100100100" |]
      "SELECT $1 + $2";
    let%map r = Pa.fetch_single_result t in
    assert (Pa.Result.nfields r = 1);
    assert (Pa.Result.ntuples r = 1);
    assert (String.equal (Pa.Result.get_value r 0 0) "9200200200")
  in
  (* Populate using a prepared statement. *)
  let shown_ntuples = 10 in
  let expected_ntuples = 4 * 100 in
  let%bind () =
    Deferred.Or_error.List.iter
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
        let%bind _ = Pa.fetch_single_result t in
        Deferred.Or_error.List.iter
          (List.range 1 100 ~stop:`inclusive)
          ~how:`Sequential
          ~f:(fun j ->
            let c0 = string_of_int (i + (3 * j)) in
            let c1 = sprintf "# %d." (i + (3 * j)) in
            let c2 = c1 in
            c#send_query_prepared ~params:[| c0; c1; c2 |] stmt;
            let%bind _ = Pa.fetch_single_result t in
            return ()))
  in
  (* Prepare a select statement. *)
  c#send_prepare "test_sel" "SELECT * FROM postgresql_ocaml_async";
  let%bind _ = Pa.fetch_single_result t in
  (* Describe it. *)
  c#send_describe_prepared "test_sel";
  let%bind r = Pa.fetch_single_result t in
  assert (Pa.Result.nfields r = 4);
  assert (Poly.equal (Pa.Result.fname r 0) "id");
  assert (Poly.equal (Pa.Result.fname r 1) "a");
  assert (Poly.equal (Pa.Result.fname r 2) "b");
  (* Run it. *)
  c#send_query_prepared "test_sel";
  let%bind r = Pa.fetch_single_result t in
  print_s [%message (Pa.Result.ntuples r : int) (Pa.Result.nfields r : int)];
  assert (Int.equal (Pa.Result.ntuples r) expected_ntuples);
  assert (Pa.Result.nfields r = 4);
  for i = 0 to min (Pa.Result.ntuples r) shown_ntuples - 1 do
    printf
      "%s, %s, %s, %s\n"
      (Pa.Result.get_value r i 0)
      (Pa.Result.get_value r i 1)
      (Pa.Result.get_value r i 2)
      (Pa.Result.get_escaped_value r i 3)
  done;
  printf "[...]\n";
  (* Run it in single-row mode. *)
  c#send_query_prepared "test_sel";
  c#set_single_row_mode;
  let%bind () =
    Deferred.Or_error.List.iter
      (List.range ~stop:`inclusive 0 expected_ntuples)
      ~how:`Sequential
      ~f:(fun i ->
        match%map Pa.fetch_result t with
        | None -> ()
        | Some r ->
          if i < shown_ntuples
          then
            printf
              "%s, %s, %s\n"
              (Pa.Result.get_value r 0 0)
              (Pa.Result.get_value r 0 1)
              (Pa.Result.get_value r 0 2))
  in
  printf "[...]\n";
  let%bind () =
    match%map Pa.fetch_result t with
    | None -> ()
    | Some _ -> assert false
  in
  (* Drop the main table. *)
  c#send_query "DROP TABLE postgresql_ocaml_async CASCADE";
  let%bind _ = Pa.fetch_single_result t in
  return ()
;;

let () =
  let required_string, required_int = Command.Param.(required string, required int) in
  let command =
    let open Command.Let_syntax in
    let%map_open host = flag "-host" required_string ~doc:" hostname for database"
    and user = flag "-user" required_string ~doc:" database username"
    and port = flag "-port" required_int ~doc:" database port" in
    fun () ->
      let%bind.Deferred.Or_error t = P_async.create ~host ~user ~port in
      let%bind.Deferred.Or_error () = test t in
      Deferred.Or_error.ok_unit
  in
  Command.run (Command.async_or_error ~summary:"Connect to pgsql" command)
;;
