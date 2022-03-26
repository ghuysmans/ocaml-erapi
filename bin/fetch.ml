module E = ExchangeRate.Make (Cohttp_lwt_unix.Client)

let () = Lwt_main.run (
  let base = Sys.argv.(1) in
  match%lwt E.fetch base with
  | Ok t ->
    Printf.eprintf "last update: %f\n" t.last_update;
    Printf.printf "cur,rate\n";
    t.rates |> List.iter (fun (cur, rate) -> Printf.printf "%s,%f\n" cur rate);
    Lwt.return ()
  | Error (`HTTP status) ->
    let status = Cohttp.Code.string_of_status status in
    Printf.eprintf "HTTP request failed with %S\n" status;
    exit 1
  | Error (`Reason reason) ->
    Printf.eprintf "API failed with %S\n" reason;
    exit 1
)
