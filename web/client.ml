open Js_of_ocaml
module E = ExchangeRate.Make (Cohttp_lwt_xhr.Client)

let () = Lwt.async (fun () ->
  match%lwt E.fetch "EUR" with
  | Ok t ->
    Printf.printf "last update: %f\n" t.last_update;
    let c' = Dom_html.(createSelect document) in
    t.rates |> List.iter (fun (cur, rate) ->
      let opt = Dom_html.(createOption document) in
      opt##.textContent := Js.(some (string cur));
      opt##.value := Js.string (string_of_float rate);
      ignore @@ c'##appendChild (opt :> Dom.node Js.t)
    );
    let i = Dom_html.(createInput ~_type:(Js.string "number") document) in
    i##.required := Js._true;
    let o = Dom_html.(createInput document) in (* FIXME number? *)
    o##.readOnly := Js._true;
    let b = Dom_html.(createButton document) in
    b##.textContent := Js.(some (string "->"));
    let f = Dom_html.(createForm document) in
    f##.onsubmit := Dom.handler (fun _ ->
      let i = float_of_string (Js.to_string i##.value) in
      let r = float_of_string (Js.to_string c'##.value) in
      o##.value := Js.string (string_of_float (i *. r));
      Js._false
    );
    ignore @@ f##appendChild (i :> Dom.node Js.t);
    ignore @@ f##appendChild (b :> Dom.node Js.t);
    ignore @@ f##appendChild (o :> Dom.node Js.t);
    ignore @@ f##appendChild (c' :> Dom.node Js.t);
    ignore @@ Dom_html.document##.body##appendChild (f :> Dom.node Js.t);
    Lwt.return ()
  | Error e ->
    let msg =
      match e with
      | `HTTP status ->
        let status = Cohttp.Code.string_of_status status in
        Printf.sprintf "HTTP request failed with %S" status
      | `Reason reason ->
        Printf.sprintf "API failed with %S" reason
    in
    let d = Dom_html.(createDiv document) in
    d##.classList##add (Js.string "err");
    d##.textContent := Js.(some (string msg));
    ignore @@ Dom_html.document##.body##appendChild (d :> Dom.node Js.t);
    Lwt.return ()
)
