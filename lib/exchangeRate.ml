type t = {
  (* float works on 32-bit hosts, too *)
  last_update: float;
  next_update: float;
  rates: (string * float) list;
}

module Make (C : Cohttp_lwt.S.Client) = struct
  let fetch base =
    let open Lwt.Infix in
    Uri.of_string ("https://open.er-api.com/v6/latest/" ^ base) |>
    C.get >>= fun (resp, body) ->
    let st = Cohttp.Response.status resp in
    if st = `OK then
      Cohttp_lwt.Body.to_string body >>= fun raw ->
      let j = Yojson.Safe.from_string raw in
      let open Yojson.Safe.Util in
      if member "result" j |> to_string = "success" then
        let t name =
          match member name j with
          | `Int x -> float x
          | `Intlit x -> float_of_string x
          | _ -> failwith "can't read time_*"
        in
        let last_update = t "time_last_update_unix" in
        let next_update = t "time_next_update_unix" in
        let eol = t "time_eol_unix" in
        if eol > 0. then
          Printf.eprintf "warning: ExchangeRate sent eol=%f\n" eol;
        let rates =
          member "rates" j |> to_assoc |>
          List.map (fun (x, y) -> x, to_number y)
        in
        Lwt.return_ok {last_update; next_update; rates}
      else
        Lwt.return_error (`Reason (member "error-type" j |> to_string))
    else
      Lwt.return_error (`HTTP st)
  end
