open Interpreter

let usage_msg = "forthish [(-e <expression>) | <filename>]"
let eval_flag = ref false
let interp_flag = ref false
let filename = ref ""
let sentence = ref ""
let args = ref []
let anon_fun name = filename := name

let speclist =
  [
    ( "-i",
      Arg.Tuple
        [
          Arg.Set interp_flag;
          Arg.Set_string filename;
          (args := [];
           Arg.Rest (fun s -> args := !args @ [ s ]));
        ],
      "Interpret the file with given arguments" );
    ( "-e",
      Tuple [ Arg.Set eval_flag; Arg.Set_string sentence ],
      "Eval the given expression" );
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  match (!eval_flag, !interp_flag) with
  | true, true -> print_endline "-e and -i cannot be used together"
  | true, false -> string_run !sentence
  | false, true -> file_run_with_args !filename !args
  | false, false -> (
      match !filename with "" -> run () | name -> file_run name)
