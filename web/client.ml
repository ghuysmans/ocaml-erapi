open Js_of_ocaml
open Emoji
module E = ExchangeRate.Make (Cohttp_lwt_xhr.Client)

let append parent child =
  ignore (parent##appendChild (child :> Dom.node Js.t))

type t = {
  iso_4217: string;
  name: string;
  translation: string;
}

let data = [
  {iso_4217 = "EGP"; name = "Piastre"; translation = "livre égyptienne"};
  {iso_4217 = "EUR"; name = "Euro"; translation = "euro"};
  {iso_4217 = "RUB"; name = "российский рубль"; translation = "rouble"};
]

let string_of_ascending asc =
  if asc then "▲" else "▼"

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
    let tbl = Dom_html.(createTable document) in
    let rows = ref (
      data |> List.map (fun x ->
        let r = Dom_html.(createTr document) in
        [
          x.iso_4217 ^ " " ^ Js.to_string (flag (String.sub x.iso_4217 0 2));
          x.name;
          x.translation;
          string_of_float @@ List.assoc x.iso_4217 t.rates;
        ] |> List.iter (fun x ->
          let c = Dom_html.(createTd document) in
          c##.textContent := Js.(some (string x));
          append r c
        );
        x, r
      )
    ) in
    let compare_map f (x, _) (y, _) = compare (f x) (f y) in
    let columns = [
      "ISO", compare_map (fun x -> x.iso_4217);
      "Nom", compare_map (fun x -> x.name);
      "Traduction", compare_map (fun x -> x.translation);
      "1€", compare_map (fun x -> List.assoc x.iso_4217 t.rates);
    ] in
    let header = Dom_html.(createTr document) in
    let spans = Array.init (List.length columns) (fun _ -> Dom_html.(createSpan document)) in
    let sort = ref None in
    let refresh () = !rows |> List.iter (fun (_, r) -> append tbl r) in
    columns |> List.iteri (fun i (heading, cmp) ->
      let th = Dom_html.(createTh document) in
      th##.textContent := Js.(some (string heading));
      th##.onclick := Dom.handler (fun _ ->
        begin match !sort with
        | Some (i', asc) when i = i' ->
          rows := List.rev !rows;
          sort := Some (i, not asc);
          spans.(i)##.textContent := Js.(some (string (string_of_ascending (not asc))))
        | _ ->
          begin match !sort with
          | Some (i', _) -> spans.(i')##.textContent := Js.null
          | None -> ()
          end;
          rows := List.sort cmp !rows;
          sort := Some (i, true);
          spans.(i)##.textContent := Js.(some (string (string_of_ascending true)))
        end;
        refresh ();
        Js._true
      );
      append th spans.(i);
      append header th
    );
    append tbl header;
    refresh ();
    append Dom_html.document##.body tbl;
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
