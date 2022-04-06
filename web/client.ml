open Js_of_ocaml
module E = ExchangeRate.Make (Cohttp_lwt_xhr.Client)

let append parent child =
  ignore (parent##appendChild (child :> Dom.node Js.t))

let () = Lwt.async (fun () ->
  match%lwt E.fetch "EUR" with
  | Ok t ->
    Printf.printf "last update: %f\n" t.last_update;
    let c' = Dom_html.(createSelect document) in
    t.rates |> List.iter (fun (cur, rate) ->
      let opt = Dom_html.(createOption document) in
      opt##.textContent := Js.(some (string cur));
      opt##.value := Js.string (string_of_float rate);
      append c' opt
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
    append f i;
    append f b;
    append f o;
    append f c';
    append Dom_html.document##.body f;
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
    append Dom_html.document##.body d;
    Lwt.return ()
)
