open Js_of_ocaml
module Html = Dom_html
open Forthish.Parser

let pop_output = function
  | Module (name, fns, modls, out, i) -> (out, Module (name, fns, modls, "", i))

let run modl str =
  try
    String.trim str |> fun s -> (String.to_seq s) () |> parse modl |> pop_output
  with
  | Eval_err (s, _) -> (s, modl)
  | Parse_err (s, _) -> (s, modl)
  | Sys_error s -> (s, modl)
  | Failure s -> (s, modl)
  | End_of_file -> ("", modl)
  | Not_found -> ("Err! No Such Definition", modl)
  | Stack.Empty -> ("Err! Empty Stack", modl)
  | Division_by_zero -> ("Err! Division by Zero", modl)
  | Exit -> ("Cannot Exit the Browser", modl)
  | _ -> ("Unknown Failure", modl)

let make_div d parent text =
  let div = Html.createDiv d in
  div##.innerHTML := text;
  Dom.appendChild parent div

let onload _ =
  let d = Html.document in
  let hist =
    Js.Opt.get
      (d##getElementById (Js.string "history"))
      (fun () -> assert false)
  in
  let interp =
    Js.Opt.get
      (d##getElementById (Js.string "interpreter"))
      (fun () -> assert false)
  in
  interp##focus;
  (* Initialize Empty Module and Import List Module *)
  let modl = ref (run (create "") "\"std/list\" useup" |> fun (_, m) -> m) in
  let match_key modl e =
    match e##.keyCode with
    | 13 ->
        (match Js.Opt.to_option interp##.textContent with
        | Some s ->
            let res, m = run !modl (Js.to_string s) in
            modl := m;
            make_div d hist
              ((Js.string "> ")##concat_2 s (Js.string ("<br/>" ^ res)));
            interp##.textContent := Js.Opt.return (Js.string "")
        | None -> ());
        Js._false
    | 76 when Js.to_bool e##.ctrlKey ->
        hist##.textContent := Js.Opt.return (Js.string "");
        Js._false
    | _ -> Js._true
  in
  ignore
  @@ Html.addEventListener interp Html.Event.keydown
       (Html.handler (match_key modl))
       Js._true;
  ignore
  @@ Html.addEventListener d Html.Event.mousedown
       (Html.handler (fun _ ->
            interp##focus;
            Js._false))
       Js._true;
  Js._false

let _ = Html.window##.onload := Html.handler onload
