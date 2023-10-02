open Forthish.Interpretor

let usage_msg = "forthish [(-e <expression>) | <filename>]"
let filename = ref ""
let sentence = ref ""
let anon_fun name = filename := name
let speclist = [ ("-e", Arg.Set_string sentence, "Eval the given expression") ]

(* At the moment Arg is overkill, but better set it up earlier than later *)
let () =
  Arg.parse speclist anon_fun usage_msg;
  match !sentence with
  | "" -> ( match !filename with "" -> run () | name -> file_run name)
  | str -> string_run str
