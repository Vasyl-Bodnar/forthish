open Printf
open Forthish.Parser

let pop_output = function
  | Module (name, fns, modls, out, i) ->
      print_endline out;
      Module (name, fns, modls, "", i)

let handle_err feval =
  try feval () with
  | Eval_err (s, Module (name, _, _, _, i)) ->
      print_string
        (s ^ " at line: " ^ string_of_int i
        ^ if name = "" then "" else " in module " ^ name)
  | Parse_err (s, Module (name, _, _, _, i)) ->
      print_string
        (s ^ " at line: " ^ string_of_int i
        ^ if name = "" then "" else " in module " ^ name)
  | Sys_error s -> print_string s
  | Failure s -> print_string s
  | End_of_file -> ()
  | Not_found -> print_string "Err! No Such Definition"
  | Stack.Empty -> print_string "Err! Empty Stack"
  | Division_by_zero -> print_string "Err! Division by Zero"
  | Exit -> print_string "Exiting..."
  | err -> raise err

let run () =
  let rec ignore_err str modl =
    print_endline str;
    flush stdout;
    inn modl
  and inn modl =
    try
      Ocamline.read ~brackets:[ ('(', ')'); ('[', ']') ] ~strings:[ '"' ] ()
      |> fun x -> String.to_seq x () |> parse modl |> pop_output |> inn
    with
    | Eval_err (s, _) -> ignore_err s modl
    | Parse_err (s, _) -> ignore_err s modl
    | Sys_error s -> ignore_err s modl
    | Failure s -> ignore_err s modl
    | End_of_file -> ignore_err "" modl
    | Not_found -> ignore_err "Err! No Such Definition" modl
    | Stack.Empty -> ignore_err "Err! Empty Stack" modl
    | Division_by_zero -> ignore_err "Err! Division by Zero" modl
  in
  try inn (create "") with Exit -> print_string "Exiting..."

let string_run str =
  (fun () ->
    let _ = parse (create "") (String.to_seq str ()) |> pop_output in
    ())
  |> handle_err

let file_run name =
  (fun () ->
    let _ = eval_file name |> pop_output in
    ())
  |> handle_err

let file_run_with_args name args =
  (fun () ->
    let modl =
      List.fold_left
        (fun (m, acc) x ->
          (parse m (String.to_seq (sprintf ": $%i \"%s\" ;" acc x) ()), acc + 1))
        (create name, 0)
        (name :: args)
      |> fun (m, _) -> m
    in
    let _ =
      open_in name |> fun ic ->
      let _ = In_channel.input_line ic in
      In_channel.input_all ic |> fun x ->
      String.to_seq x () |> parse modl |> pop_output
    in
    ())
  |> handle_err
