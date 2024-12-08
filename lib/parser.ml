type def =
  | Int
  | Str
  | Bool
  | Gen of char
  | Fuzzy
  | Quote of def
  | Multiple of def list
  | Fun of def * def

let rec print_def = function
  | Int -> print_string "int"
  | Str -> print_string "str"
  | Bool -> print_string "bool"
  | Fuzzy -> print_char '?'
  | Gen c -> print_char c
  | Quote d ->
      print_string "quote ";
      print_def d
  | Multiple m -> List.iter print_def m
  | Fun (i, o) ->
      print_def i;
      print_def o

type lit = Int of int | Str of string | Bool of bool

type token =
  | Literal of lit
  | Word of string
  | Quote of token list
  | Define of string * def option * token list
  | ModDefine of string * token list
  | ModCall of string list * string
  | Comment of string

let rec string_of_tok = function
  | Literal (Int i) -> string_of_int i
  | Literal (Str s) -> s
  | Literal (Bool b) -> string_of_bool b
  | Word w -> w
  | Quote q ->
      List.(map string_of_tok q |> fold_left (fun acc x -> acc ^ " " ^ x) "[")
      ^ " ]"
  | Define (n, _, ts) ->
      n
      ^ List.(
          rev_map string_of_tok ts |> fold_left (fun acc x -> acc ^ " " ^ x) ":")
  | ModDefine (n, ts) ->
      n
      ^ List.(
          map string_of_tok ts |> fun ss ->
          fold_right (fun acc x -> acc ^ " " ^ x) ss ":")
  | ModCall (sl, s) -> sl @ [ s ] |> String.concat ":"
  | Comment c -> c

let rec stringify_tok = function
  | Literal (Int i) -> string_of_int i ^ " "
  | Literal (Str s) -> "\"" ^ s ^ "\" "
  | Literal (Bool b) -> string_of_bool b ^ " "
  | Word w -> w ^ " "
  | Quote q -> "[ " ^ (List.map stringify_tok q |> String.concat " ") ^ " ]"
  | Define (n, _, ts) ->
      ": " ^ n ^ " "
      ^ (String.concat "" @@ List.rev_map stringify_tok ts)
      ^ " ;\n"
  | ModDefine (n, ts) ->
      n ^ ": " ^ (String.concat "" @@ List.rev_map stringify_tok ts) ^ " ;\n"
  | ModCall (sl, s) -> (sl @ [ s ] |> String.concat ":") ^ " "
  | Comment c -> "( " ^ c ^ ") "

let rec print_tok = function
  | Literal (Int i) ->
      print_int i;
      print_string " "
  | Literal (Str s) -> print_string @@ "\"" ^ s ^ "\" "
  | Literal (Bool b) -> print_string @@ if b then "true " else "false "
  | Word w -> print_string @@ w ^ " "
  | Quote q ->
      print_string "[ ";
      List.iter print_tok q;
      print_string "] "
  | Define (n, d, ts) ->
      print_string @@ ":" ^ n ^ " ";
      (match d with
      | Some d ->
          print_string "( ";
          print_def d;
          print_string " ) "
      | _ -> ());
      List.rev ts |> List.iter print_tok;
      print_string "; "
  | ModDefine (n, ts) ->
      print_string @@ n ^ ": ";
      List.rev ts |> List.iter print_tok;
      print_string "; "
  | ModCall (sl, s) ->
      sl @ [ s ] |> String.concat ":" |> print_string;
      print_string ""
  | Comment c ->
      print_string "( ";
      print_string c;
      print_string " )"

(* Printing token list from functions *)
let rec print_fun ?(tabul = false) ?(tab = 0) =
  if tabul then print_string (String.make tab ' ');
  function
  | [] -> ()
  | Literal (Int i) :: ts ->
      print_int i;
      print_string " ";
      print_fun ~tab ts
  | Literal (Str s) :: ts ->
      print_string @@ "\"" ^ s ^ "\" ";
      print_fun ~tab ts
  | Literal (Bool b) :: ts ->
      print_string @@ if b then "true " else "false ";
      print_fun ~tab ts
  | Word w :: ts ->
      print_string @@ w ^ " ";
      print_fun ~tab ts
  | Quote q :: ts ->
      print_string "[ ";
      List.iter print_tok q;
      print_string "] ";
      print_fun ~tab ts
  | Define (n, d, ts) :: ts' ->
      print_string @@ String.make tab ' ' ^ ":" ^ n ^ " ";
      (match d with
      | Some d ->
          print_string "( ";
          print_def d;
          print_string " ) "
      | _ -> ());
      List.rev ts |> print_fun;
      print_string ";\n";
      print_fun ~tab ts'
  | ModDefine (n, ts) :: ts' ->
      print_string @@ String.make tab ' ' ^ n ^ ": ";
      List.rev ts |> print_fun;
      print_string ";\n";
      print_fun ~tab ts'
  | ModCall (sl, s) :: ts ->
      sl @ [ s ] |> String.concat ":" |> print_string;
      print_string " ";
      print_fun ~tab ts
  | Comment c :: ts ->
      print_string "( ";
      print_string c;
      print_string " )\n";
      print_fun ~tab ts

open Seq

module Module = struct
  type fdef = string * token list
  type t = Module of string * fdef list * t list * string * int

  exception Parse_err of string * t
  exception Eval_err of string * t

  (* Name, Function definitions, Submodules, Output, Line*)
  let create name = Module (name, [], [], "", 0)

  let useup (Module (name_a, lst_a, modls_a, out, i))
      (Module (_, lst_b, modls_b, modout, _)) =
    Module (name_a, lst_b @ lst_a, modls_b @ modls_a, modout ^ out, i)

  let use (Module (name, lst, modls, out, i)) modl =
    Module (name, lst, modl :: modls, out, i)

  let add (Module (name, lst, modls, out, i)) def =
    Module (name, def :: lst, modls, out, i)

  let remove (Module (name, lst, modls, out, i)) def_name =
    Module (name, List.remove_assoc def_name lst, modls, out, i)

  let find (Module (_, lst, _, _, _)) def_name = List.assoc def_name lst
  let find_opt (Module (_, lst, _, _, _)) def_name = List.assoc_opt def_name lst

  let find_modl (Module (_, _, modls, _, _)) modl_name =
    List.find (fun (Module (name, _, _, _, _)) -> name = modl_name) modls

  let mem (Module (_, lst, _, _, _)) def_name = List.mem_assoc def_name lst

  let incl (Module (name, lst, modls, out, i)) =
    Module (name, lst, modls, out, succ i)

  let add_out (Module (name, lst, modls, out, i)) print =
    Module (name, lst, modls, out ^ print, i)

  let replace_out (Module (name, lst, modls, _, i)) print =
    Module (name, lst, modls, print, i)

  let eval_err (Module (n, l, m, o, i)) s =
    raise (Eval_err (s, Module (n, l, m, o, succ i)))

  let parse_err (Module (n, l, m, o, i)) s =
    raise (Parse_err (s, Module (n, l, m, o, succ i)))
  (* let print_def (str, ts) = print_string (str ^ " "); List.iter print_token ts
     let rec print_module (Module (name, lst, modls)) = print_string (name ^ " "); List.iter print_def lst; List.iter print_module modls *)
end

include Module

let print_builtin_fun name modl =
  let inn = function
    | "infile" -> "s/filename -> s/filecontent"
    | "outfile" -> "s/filecontent s/filename -> !"
    | "bind" -> "q[v ..] -> !"
    | "describe" -> "q[v] | s/v -> s"
    | "use" -> "s/filename -> !"
    | "useup" -> "s/filename -> !"
    | "eval" -> "s/code -> !!"
    | "del" -> "s/word -> !"
    | "open" -> "q -> !!"
    | "hd" -> "q -> a"
    | "tl" -> "q -> q"
    | "shd" -> "q -> a"
    | "stl" -> "q -> q"
    | "quote" -> "a -> q[a]"
    | "+quote" -> "a q -> q[a ..]"
    | "if" -> "q q b -> !!"
    | "dup" -> "a -> a a"
    | "over" -> "a b -> a b a"
    | "rot" -> "a b c -> b c a"
    | "pop" -> "a -> !"
    | "swap" -> "a b -> b a"
    | "not" -> "? -> ?"
    | "len" -> "-> i"
    | "and" -> "? ? -> ?"
    | "or" -> "? ? -> ?"
    | "chars" -> "s -> qs"
    | "concat" -> "qs -> s"
    | "=" -> "a a -> ?"
    | "<>" -> "a a -> ?"
    | ">" -> "a a -> ?"
    | "<" -> "a a -> ?"
    | "+" -> "a a -> a"
    | "-" -> "a a -> a"
    | "*" -> "a a -> a"
    | "**" -> "a a -> a"
    | "/" -> "a a -> a"
    | "%" -> "a a -> a"
    | "." -> "a -> !"
    | "," -> "! -> s"
    | "exit" -> "-> !"
    | _ -> eval_err modl @@ "Function " ^ name ^ " description not found"
  in
  let typ = inn name in
  print_string @@ "Core:" ^ name ^ "( " ^ typ ^ " ) "

let stack : token Stack.t = Stack.create ()
let push x = Stack.push x stack

let pop modl n =
  match Stack.pop_opt stack with
  | Some v -> v
  | None ->
      eval_err modl
        ("Function " ^ n
       ^ " expected another Argument, but none are on the Stack")

let top modl n =
  match Stack.top_opt stack with
  | Some v -> v
  | None ->
      eval_err modl
        ("Function " ^ n
       ^ " expected another Argument, but none are on the Stack")

let err a modl s =
  push a;
  eval_err modl s

let match_bool f modl name =
  match pop modl name with
  | Literal (Bool b) -> f b
  | a -> err a modl ("Function " ^ name ^ " expected a Bool Argument")

let match_int f modl name =
  match pop modl name with
  | Literal (Int x) -> f x
  | a -> err a modl ("Function " ^ name ^ " expected an Int Argument")

let match_str f modl name =
  match pop modl name with
  | Literal (Str s) -> f s
  | a -> err a modl ("Function " ^ name ^ " expected a String Argument")

let match_quote f modl name =
  match pop modl name with
  | Quote q -> f q
  | a -> err a modl ("Function " ^ name ^ " expected a Quote Argument")

let match_nonempty_quote f modl name =
  match pop modl name with
  | Quote [] ->
      eval_err modl ("Function " ^ name ^ " expected a Non-Empty Quote Argument")
  | Quote q -> f q
  | a -> err a modl ("Function " ^ name ^ " expected a Non-Empty Quote Argument")

let bop f modl name =
  match_bool
    (fun y -> match_bool (fun x -> push @@ Literal (Bool (f x y))) modl name)
    modl name

let iop f modl name =
  match_int
    (fun y -> match_int (fun x -> push @@ Literal (Int (f x y))) modl name)
    modl name

let isqop f g h modl name =
  match pop modl name with
  | Literal (Int y) ->
      match_int (fun x -> push @@ Literal (Int (f x y))) modl name
  | Literal (Str d) ->
      match_str (fun s -> push @@ Literal (Str (g s d))) modl name
  | Quote q -> match_quote (fun e -> push @@ Quote (h e q)) modl name
  | a -> err a modl ("Function " ^ name ^ " expected either Ints or Strings")

let isop_bool f g modl name =
  match pop modl name with
  | Literal (Int y) ->
      match_int (fun x -> push @@ Literal (Bool (f x y))) modl name
  | Literal (Str d) ->
      match_str (fun s -> push @@ Literal (Bool (g s d))) modl name
  | a -> err a modl ("Function " ^ name ^ " expected either Ints or Strings")

let isbqop_bool f g h j modl name =
  match pop modl name with
  | Literal (Int y) ->
      match_int (fun x -> push @@ Literal (Bool (f x y))) modl name
  | Literal (Str d) ->
      match_str (fun s -> push @@ Literal (Bool (g s d))) modl name
  | Literal (Bool n) ->
      match_bool (fun b -> push @@ Literal (Bool (h b n))) modl name
  | Quote q -> match_quote (fun p -> push @@ Literal (Bool (j p q))) modl name
  | a ->
      err a modl ("Function " ^ name ^ " expected either Ints, Strings or Bools")

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let uncurry f (a, b) = f a b
let uncurry3 f (a, b, c) = f a b c
let uncurry4 f (a, b, c, d) = f a b c d
let map_a3 f (a, b, c) = (f a, b, c)
let map_a4 f (a, b, c, d) = (f a, b, c, d)
let prep_a3 (a, b, c) d = (Cons (d, fun () -> a), b, c)
let prep_a4 (a, b, c, d) e = (Cons (e, fun () -> a), b, c, d)

let int modl cs =
  let rec inn modl = function
    | Cons (('0' .. '9' as i), cs) -> prep_a3 (inn modl (cs ())) i
    | Cons (' ', cs) -> (Nil, modl, cs ())
    | Cons ('\n', cs) -> (Nil, incl modl, cs ())
    | cs -> (Nil, modl, cs)
  in
  inn modl cs
  |> map_a3 (fun seq ->
         Literal
           (Int
              (String.of_seq (fun () -> seq) |> int_of_string_opt |> function
               | Some v -> v
               | None -> parse_err modl "Number is too large")))

let comm modl cs =
  let rec inn acc modl = function
    | Cons ('(', cs) -> inn acc modl (cs ()) |> uncurry3 inn
    | Cons (')', cs) -> (List.rev acc, modl, cs ())
    | Cons ('\n', cs) -> inn acc (incl modl) (cs ())
    | Cons (c, cs) -> inn (c :: acc) modl (cs ())
    | Nil -> parse_err modl "Expected end of comment, not EOF"
  in
  inn [] modl (cs ())
  |> map_a3 (fun seq -> Comment (List.to_seq seq |> String.of_seq))

let str modl cs =
  let rec inn acc modl = function
    | Cons ('\\', cs) -> (
        match cs () with
        | Cons ('"', cs) -> inn ('"' :: acc) modl (cs ())
        | Cons ('0', cs) -> inn ('\000' :: acc) modl (cs ())
        | Cons ('a', cs) -> inn ('\007' :: acc) modl (cs ())
        | Cons ('b', cs) -> inn ('\008' :: acc) modl (cs ())
        | Cons ('t', cs) -> inn ('\t' :: acc) modl (cs ()) (* \009 *)
        | Cons ('n', cs) -> inn ('\n' :: acc) modl (cs ()) (* \010 *)
        | Cons ('v', cs) -> inn ('\011' :: acc) modl (cs ())
        | Cons ('f', cs) -> inn ('\012' :: acc) modl (cs ())
        | Cons ('r', cs) -> inn ('\013' :: acc) modl (cs ())
        | Cons ('\\', cs) -> inn ('\\' :: acc) modl (cs ())
        | Cons (c, cs) -> inn (c :: acc) modl (cs ())
        | Nil -> parse_err modl "Expected end of string, not EOF")
    | Cons ('"', cs) -> (List.rev acc, modl, cs ())
    | Cons ('\n', cs) -> inn acc (incl modl) (cs ())
    | Cons (c, cs) -> inn (c :: acc) modl (cs ())
    | Nil -> parse_err modl "Expected end of string, not EOF"
  in
  inn [] modl (cs ())
  |> map_a3 (fun seq -> Literal (Str (List.to_seq seq |> String.of_seq)))

let rec tru modl cs =
  if
    Seq.compare
      (fun a b -> if a = b then 0 else 1)
      (take 4 (fun () -> cs))
      (String.to_seq "true")
    = 0
  then (Literal (Bool true), modl, drop 4 (fun () -> cs) ())
  else word modl cs

and fals modl cs =
  if
    Seq.compare
      (fun a b -> if a = b then 0 else 1)
      (take 5 (fun () -> cs))
      (String.to_seq "false")
    = 0
  then (Literal (Bool false), modl, drop 5 (fun () -> cs) ())
  else word modl cs

and word modl cs =
  let app_a_inn (a, b, c, d) e =
    ( (match a with
      | Cons (c, cs) -> Cons (Cons (e, fun () -> c), cs)
      | Nil -> Cons (Cons (e, empty), empty)),
      b,
      c,
      d )
  in
  let rec inn modl = function
    | Cons (':', cs)
      when match cs () with Cons ((' ' | '\t'), _) | Nil -> true | _ -> false ->
        (Nil, modl, cs (), true)
    | Cons (':', cs)
      when match cs () with Cons (('\n' | ']' | ';'), _) -> true | _ -> false ->
        (Nil, modl, cs (), true)
    | Cons (':', cs) -> prep_a4 (inn modl (cs ())) Nil
    | Cons ((' ' | '\t'), cs) -> (Nil, modl, cs (), false)
    | Cons (('\n' | ']' | ';'), _) as cs -> (Nil, modl, cs, false)
    | Cons (c, cs) -> app_a_inn (inn modl (cs ())) c
    | cs -> (Nil, modl, cs, false)
  in
  inn modl cs |> function
  | Cons (Nil, cs), _, _, _ when cs () = Nil ->
      parse_err modl "Empty Module Configuration"
  | Cons (c, cs), modl, rest, true when cs () = Nil ->
      build ';'
        (function
          | lst -> ModDefine (String.of_seq (fun () -> c), List.rev lst))
        modl
        (fun () -> rest)
  | Cons (c, cs), modl, rest, false when cs () = Nil ->
      (Word (String.of_seq (fun () -> c)), modl, rest)
  | cs, modl, rest, _ -> (
      Seq.fold_left
        (fun acc x -> String.of_seq (fun () -> x) :: acc)
        []
        (fun () -> cs)
      |> function
      | w :: ws -> (ModCall (ws, w), modl, rest)
      | [] -> parse_err modl "Unexpected Word Configuration")

and build ch f modl cs =
  let rec inn acc modl =
    let stream f acc cs =
      f modl cs |> fun (tok, modl, cs) -> (tok :: acc, modl, cs) |> uncurry3 inn
    in
    function
    | Cons (c, cs) when c = ch -> (List.rev acc, modl, cs ())
    | Cons ('[', cs) -> stream (build ']' (fun lst -> Quote lst)) acc cs
    | Cons (':', cs) ->
        stream
          (build ';' (function
            | t :: lst -> Define (string_of_tok t, None, List.rev lst)
            | _ -> parse_err modl "Bad function definition"))
          acc cs
    | Cons ('(', cs) -> stream comm acc cs
    | Cons ('"', cs) -> stream str acc cs
    | Cons ('t', _) as cs -> stream tru acc cs
    | Cons ('f', _) as cs -> stream fals acc cs
    | Cons ('0' .. '9', _) as cs -> stream int acc cs
    | Cons (' ', cs) -> inn acc modl (cs ())
    | Cons ('\n', cs) -> inn acc (incl modl) (cs ())
    | Nil ->
        parse_err modl @@ "Expected end character \"" ^ String.make 1 ch
        ^ "\", not EOF"
    | cs -> stream word acc cs
  in
  inn [] modl (cs ()) |> map_a3 f

let locs : string list = Stdl.Sites.forthish_std

(*
let lookup_dirs dirs =
  List.filter Sys.file_exists dirs
  |> List.map (fun dir -> Array.to_list (Sys.readdir dir))
  |> List.concat
*)

(* let find_available () = lookup_dirs locs *)

let lookup_file filename dirs =
  List.find_map
    (fun dir ->
      let filename' = Filename.concat dir filename in
      if Sys.file_exists filename' then Some filename' else None)
    dirs

let rec parse (modl : Module.t) =
  let stream f cs =
    f modl cs |> fun (tok, modl, cs) -> (eval modl tok, cs) |> uncurry parse
  in
  function
  | Cons ('[', cs) -> stream (build ']' (fun lst -> Quote lst)) cs
  | Cons (':', cs) ->
      stream
        (build ';' (function
          | t :: lst -> Define (string_of_tok t, None, List.rev lst)
          | _ -> parse_err modl "Bad function definition"))
        cs
  | Cons ('(', cs) -> stream comm cs
  | Cons ('"', cs) -> stream str cs
  | Cons ('t', _) as cs -> stream tru cs
  | Cons ('f', _) as cs -> stream fals cs
  | Cons ('0' .. '9', _) as cs -> stream int cs
  | Cons (' ', cs) -> parse modl (cs ())
  | Cons ('\n', cs) -> parse (incl modl) (cs ())
  | Nil -> modl
  | cs -> stream word cs

and eval (modl : Module.t) =
  let slide f =
    f;
    modl
  in
  let eval_list modl = List.fold_left eval modl in
  let rec stl = function _ :: [] | [] -> [] | x :: xs -> x :: stl xs in
  let spaced = function
    | s when String.ends_with ~suffix:"\t" s -> s
    | s when String.ends_with ~suffix:"\n" s -> s
    | s when String.ends_with ~suffix:"\011" s -> s
    | s when String.ends_with ~suffix:"\012" s -> s
    | s when String.ends_with ~suffix:"\013" s -> s
    | s -> s ^ " "
  in
  function
  | Literal _ as x -> slide (push x)
  | Quote _ as q -> slide (push q)
  | Comment _ -> modl
  | ModCall (sl, s) ->
      List.rev sl |> List.fold_left find_modl modl |> fun m ->
      let (Module (_, _, _, out, _)) = eval m (Word s) in
      add_out modl out
  | Word w -> (
      match w with
      | w when mem modl w -> (
          find modl w |> eval_list modl |> function
          | Module (_, _, _, out, _) -> replace_out modl out)
      | "infile" ->
          match_str
            (fun s ->
              slide (push @@ Literal (Str (open_in s |> In_channel.input_all))))
            modl w
      | "outfile" ->
          match_str
            (fun s ->
              slide
                (pop modl w |> string_of_tok
                |> (open_out s |> Out_channel.output_string)))
            modl w
      | "bind" ->
          match_nonempty_quote
            (fun q ->
              match q with
              | [ Word n ] -> add modl (n, [ Quote [ Word n ] ])
              | Word n :: ts -> add modl (n, ts)
              | _ -> eval_err modl "Expected a Quote of Word and Tokens")
            modl w
      | "describe" ->
          slide
            (match pop modl w with
            | Quote [ Word n ] | Literal (Str n) -> (
                match find_opt modl n with
                | Some ts -> print_fun ts
                | None -> print_builtin_fun n modl)
            | _ -> eval_err modl "Expected a Word or String of a Word")
      | "desc" ->
          slide
            (match pop modl w with
            | Quote [ Word n ] | Literal (Str n) -> (
                match find_opt modl n with
                | Some ts -> push @@ Quote ts
                | None -> push @@ Literal (Str n))
            | _ -> eval_err modl "Expected a Word or String of a Word")
      | "use" -> match_str (fun s -> eval_file s |> use modl) modl w
      | "useup" -> match_str (fun s -> eval_file s |> useup modl) modl w
      | "eval" -> match_str (fun s -> parse modl (String.to_seq s ())) modl w
      | "del" -> match_str (remove modl) modl w
      | "open" -> match_quote (eval_list modl) modl w
      | "hd" -> match_nonempty_quote (fun q -> List.hd q |> eval modl) modl w
      | "tl" ->
          match_nonempty_quote (fun q -> eval modl (Quote (List.tl q))) modl w
      | "shd" ->
          match_nonempty_quote
            (fun q -> List.rev q |> List.hd |> eval modl)
            modl w
      | "stl" ->
          match_nonempty_quote (fun q -> eval modl (Quote (stl q))) modl w
      | "quote" -> slide (push @@ Quote [ pop modl w ])
      | "+quote" ->
          slide (match_quote (fun q -> push @@ Quote (pop modl w :: q)) modl w)
      | "if" ->
          match_quote
            (fun f ->
              match_quote
                (fun t ->
                  match_bool
                    (fun b -> if b then eval_list modl t else eval_list modl f)
                    modl w)
                modl w)
            modl w
      | "dup" -> slide (push @@ top modl w)
      | "over" ->
          slide
            (let a = pop modl w in
             let b = top modl w in
             push a;
             push b)
      | "rot" ->
          slide
            (let a = pop modl w in
             let b = pop modl w in
             let c = pop modl w in
             push b;
             push a;
             push c)
      | "pop" ->
          slide
            (let _ = pop modl w in
             ())
      | "swap" ->
          slide
            (let a = pop modl w in
             let b = pop modl w in
             push a;
             push b)
      | "not" ->
          slide (match_bool (fun x -> push @@ Literal (Bool (not x))) modl w)
      | "and" -> slide (bop ( && ) modl w)
      | "or" -> slide (bop ( || ) modl w)
      | "len" -> slide (push @@ Literal (Int (Stack.length stack)))
      | "chars" ->
          match_str
            (fun s ->
              String.to_seq s
              |> Seq.map (fun c -> Literal (Str (String.make 1 c)))
              |> List.of_seq
              |> fun x -> Quote x |> push |> slide)
            modl w
      | "concat" ->
          match_quote
            (fun q ->
              List.map
                (function
                  | Literal (Str s) -> s
                  | _ -> eval_err modl "Expected a Quote of Strings or Chars")
                q
              |> String.concat ""
              |> fun s -> Literal (Str s) |> push |> slide)
            modl w
      | "=" ->
          slide
            (isbqop_bool ( = ) ( = ) ( = )
               (fun x y -> List.compare Stdlib.compare x y = 0)
               modl w)
      | "<>" ->
          slide
            (isbqop_bool ( <> )
               (fun x y -> not (String.equal x y))
               ( <> )
               (fun x y -> List.compare Stdlib.compare x y <> 0)
               modl w)
      | ">" ->
          slide (isop_bool ( > ) (fun x y -> String.compare x y = 1) modl w)
      | "<" ->
          slide (isop_bool ( < ) (fun x y -> String.compare x y = -1) modl w)
      | "+" -> slide (isqop ( + ) ( ^ ) ( @ ) modl w)
      | "-" -> slide (iop ( - ) modl w)
      | "*" -> slide (iop ( * ) modl w)
      | "**" -> slide (iop pow modl w)
      | "/" -> slide (iop ( / ) modl w)
      | "%" -> slide (iop ( mod ) modl w)
      | "." -> add_out modl (spaced (string_of_tok (pop modl w)))
      | "," -> slide (push @@ Literal (Str (read_line ())))
      | "exit" -> raise Exit
      | _ -> eval_err modl @@ "Undefined Word \"" ^ w ^ "\"")
  | Define (n, _, ts) ->
      List.fold_left
        (fun acc -> function
          | Word "take" -> pop modl "take" :: acc
          | a -> a :: acc)
        [] ts
      |> fun ts -> add modl (n, ts)
  | ModDefine (n, ts) ->
      List.fold_right (fun x y -> eval y x) ts (useup (create n) modl)
      |> use modl

and eval_file filename =
  let input =
    let filename =
      if Filename.extension filename = ".fthish" then filename
      else filename ^ ".fthish"
    in
    if Sys.file_exists filename then open_in filename
    else
      match lookup_file filename locs with
      | Some f -> open_in f
      | None -> raise (Eval_err ("File " ^ filename ^ " not found", create ""))
  in
  input |> In_channel.input_all |> fun x ->
  String.to_seq x ()
  |> parse
       (create
          (Filename.basename filename |> Filename.remove_extension
         |> String.capitalize_ascii))
