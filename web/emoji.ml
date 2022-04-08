open Js_of_ocaml

let fromCodePoint (x : int) : Js.js_string Js.t =
  Js.Unsafe.(fun_call (js_expr "String.fromCodePoint") [|inject x|])

let concat (x : Js.js_string Js.t) (y : Js.js_string Js.t) : Js.js_string Js.t =
  Js.Unsafe.(fun_call (js_expr "function(x,y){return x+y;}") [|inject x; inject y|])

let flag iso =
  if String.length iso = 2 then
    if iso = String.uppercase_ascii iso then
      let f c = fromCodePoint (int_of_char c + 127397) in
      concat (f iso.[0]) (f iso.[1])
    else
      raise (Invalid_argument "non-uppercase country code")
  else
    raise (Invalid_argument "invalid country code length")
