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
  | Quote of spanned list
  | Define of string * def option * spanned list
  | ModDefine of string * spanned list
  | ModCall of string list * string
  | Comment of string
  | Error

and spanned = token * string * int * int (* Token, Module Name, Line, Column *)

let rec string_of_tok tok =
  let tok, _, _, _ = tok in
  match tok with
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
  | Error -> "Error"

let rec stringify_tok tok =
  let tok, _, _, _ = tok in
  match tok with
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
  | Error -> "Error "

let rec print_tok tok =
  let tok, _, _, _ = tok in
  match tok with
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
  | Error -> print_string "Error "

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
      List.rev_map (fun (a, _, _, _) -> a) ts |> print_fun;
      print_string ";\n";
      print_fun ~tab ts'
  | ModDefine (n, ts) :: ts' ->
      print_string @@ String.make tab ' ' ^ n ^ ": ";
      List.rev_map (fun (a, _, _, _) -> a) ts |> print_fun;
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
  | Error :: ts ->
      print_string "Error ";
      print_fun ~tab ts

open Seq

module Module = struct
  type fdef = string * spanned list
  type t = Module of string * fdef list * t list * string * int * int

  exception IO_err of string
  exception Parse_err of string * t * spanned
  exception Eval_err of string * t * spanned

  (* Name, Function definitions, Submodules, Output, Line*)
  let create name = Module (name, [], [], "", 1, 1)

  let useup (Module (name_a, lst_a, modls_a, out, i, j))
      (Module (_, lst_b, modls_b, modout, _, _)) =
    Module (name_a, lst_b @ lst_a, modls_b @ modls_a, modout ^ out, i, j)

  let use (Module (name, lst, modls, out, i, j)) modl =
    Module (name, lst, modl :: modls, out, i, j)

  let add (Module (name, lst, modls, out, i, j)) def =
    Module (name, def :: lst, modls, out, i, j)

  let remove (Module (name, lst, modls, out, i, j)) def_name =
    Module (name, List.remove_assoc def_name lst, modls, out, i, j)

  let find (Module (_, lst, _, _, _, _)) def_name = List.assoc def_name lst

  let find_opt (Module (_, lst, _, _, _, _)) def_name =
    List.assoc_opt def_name lst

  let find_modl (Module (_, _, modls, _, _, _)) modl_name =
    List.find (fun (Module (name, _, _, _, _, _)) -> name = modl_name) modls

  let mem (Module (_, lst, _, _, _, _)) def_name = List.mem_assoc def_name lst

  let add_out (Module (name, lst, modls, out, i, j)) print =
    Module (name, lst, modls, out ^ print, i, j)

  let replace_out (Module (name, lst, modls, _, i, j)) print =
    Module (name, lst, modls, print, i, j)

  let incl (Module (name, lst, modls, out, i, _)) =
    Module (name, lst, modls, out, succ i, 1)

  let incc (Module (name, lst, modls, out, i, j)) =
    Module (name, lst, modls, out, i, succ j)

  let spanify (Module (name, _, _, _, i, j)) (tok : token) = (tok, name, i, j)

  let eval_err (Module (n, l, m, o, i, j)) s span =
    raise (Eval_err (s, Module (n, l, m, o, i, j), span))

  let parse_err (Module (n, l, m, o, i, j)) s span =
    raise (Parse_err (s, Module (n, l, m, o, i, j), span))
end

include Module

let print_builtin_fun name modl span =
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
    | _ -> eval_err modl ("Function " ^ name ^ " description not found") span
  in
  let typ = inn name in
  print_string @@ "Core:" ^ name ^ "( " ^ typ ^ " ) "

let stack : token Stack.t = Stack.create ()
let push x = Stack.push x stack

let pop modl n span =
  match Stack.pop_opt stack with
  | Some v -> v
  | None ->
      eval_err modl
        ("Function " ^ n
       ^ " expected another Argument, but none are on the Stack")
        span

let top modl n span =
  match Stack.top_opt stack with
  | Some v -> v
  | None ->
      eval_err modl
        ("Function " ^ n
       ^ " expected another Argument, but none are on the Stack")
        span

let err a modl s span =
  push a;
  eval_err modl s span

let match_bool f modl name span =
  match pop modl name span with
  | Literal (Bool b) -> f b
  | a -> err a modl ("Function " ^ name ^ " expected a Bool Argument") span

let match_int f modl name span =
  match pop modl name span with
  | Literal (Int x) -> f x
  | a -> err a modl ("Function " ^ name ^ " expected an Int Argument") span

let match_str f modl name span =
  match pop modl name span with
  | Literal (Str s) -> f s
  | a -> err a modl ("Function " ^ name ^ " expected a String Argument") span

let match_quote f modl name span =
  match pop modl name span with
  | Quote q -> f q
  | a -> err a modl ("Function " ^ name ^ " expected a Quote Argument") span

let match_nonempty_quote f modl name span =
  match pop modl name span with
  | Quote [] ->
      eval_err modl
        ("Function " ^ name ^ " expected a Non-Empty Quote Argument")
        span
  | Quote q -> f q
  | a ->
      err a modl
        ("Function " ^ name ^ " expected a Non-Empty Quote Argument")
        span

let bop f modl name span =
  match_bool
    (fun y ->
      match_bool (fun x -> push @@ Literal (Bool (f x y))) modl name span)
    modl name span

let iop f modl name span =
  match_int
    (fun y -> match_int (fun x -> push @@ Literal (Int (f x y))) modl name span)
    modl name span

let isqop f g h modl name span =
  match pop modl name span with
  | Literal (Int y) ->
      match_int (fun x -> push @@ Literal (Int (f x y))) modl name span
  | Literal (Str d) ->
      match_str (fun s -> push @@ Literal (Str (g s d))) modl name span
  | Quote q -> match_quote (fun e -> push @@ Quote (h e q)) modl name span
  | a ->
      err a modl ("Function " ^ name ^ " expected either Ints or Strings") span

let isop_bool f g modl name span =
  match pop modl name span with
  | Literal (Int y) ->
      match_int (fun x -> push @@ Literal (Bool (f x y))) modl name span
  | Literal (Str d) ->
      match_str (fun s -> push @@ Literal (Bool (g s d))) modl name span
  | a ->
      err a modl ("Function " ^ name ^ " expected either Ints or Strings") span

let isbqop_bool f g h j modl name span =
  match pop modl name span with
  | Literal (Int y) ->
      match_int (fun x -> push @@ Literal (Bool (f x y))) modl name span
  | Literal (Str d) ->
      match_str (fun s -> push @@ Literal (Bool (g s d))) modl name span
  | Literal (Bool n) ->
      match_bool (fun b -> push @@ Literal (Bool (h b n))) modl name span
  | Quote q ->
      match_quote (fun p -> push @@ Literal (Bool (j p q))) modl name span
  | a ->
      err a modl
        ("Function " ^ name ^ " expected either Ints, Strings or Bools")
        span

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
    | Cons (('0' .. '9' as i), cs) -> prep_a3 (inn (incc modl) (cs ())) i
    | Cons (' ', cs) -> (Nil, incc modl, cs ())
    | Cons ('\n', cs) -> (Nil, incl modl, cs ())
    | cs -> (Nil, incc modl, cs)
  in
  inn modl cs
  |> map_a3 (fun seq ->
         spanify modl
         @@ Literal
              (Int
                 (String.of_seq (fun () -> seq) |> int_of_string_opt |> function
                  | Some v -> v
                  | None ->
                      parse_err modl "Number is too large"
                        (spanify modl
                           (Literal (Str (String.of_seq (fun () -> seq))))))))

let comm modl cs =
  let rec inn acc modl = function
    | Cons ('(', cs) -> inn acc (incc modl) (cs ()) |> uncurry3 inn
    | Cons (')', cs) -> (List.rev acc, incc modl, cs ())
    | Cons ('\n', cs) -> inn acc (incl modl) (cs ())
    | Cons (c, cs) -> inn (c :: acc) (incc modl) (cs ())
    | Nil ->
        parse_err modl "Expected end of comment, not EOF" (spanify modl Error)
  in
  inn [] modl (cs ())
  |> map_a3 (fun seq ->
         spanify modl @@ Comment (List.to_seq seq |> String.of_seq))

let str modl cs =
  let rec inn acc modl = function
    | Cons ('\\', cs) ->
        inn
          (match cs () with
          | Cons ('"', _) -> '"' :: acc
          | Cons ('0', _) -> '\000' :: acc
          | Cons ('a', _) -> '\007' :: acc
          | Cons ('b', _) -> '\008' :: acc
          | Cons ('t', _) -> '\t' :: acc (* \009 *)
          | Cons ('n', _) -> '\n' :: acc (* \010 *)
          | Cons ('v', _) -> '\011' :: acc
          | Cons ('f', _) -> '\012' :: acc
          | Cons ('r', _) -> '\013' :: acc
          | Cons ('\\', _) -> '\\' :: acc
          | Cons (c, _) -> c :: acc
          | Nil ->
              parse_err modl "Expected end of string, not EOF"
                (spanify modl
                @@ Literal (Str (List.to_seq acc |> String.of_seq))))
          (incc modl)
          (match cs () with
          | Cons (_, cs) -> cs ()
          | _ ->
              parse_err modl "Expected end of string, not EOF"
                (spanify modl Error))
    | Cons ('"', cs) -> (List.rev acc, incc modl, cs ())
    | Cons ('\n', cs) -> inn acc (incl modl) (cs ())
    | Cons (c, cs) -> inn (c :: acc) (incc modl) (cs ())
    | Nil ->
        parse_err modl "Expected end of string, not EOF" (spanify modl Error)
  in
  inn [] modl (cs ())
  |> map_a3 (fun seq ->
         spanify modl @@ Literal (Str (List.to_seq seq |> String.of_seq)))

let rec tru modl cs =
  if
    Seq.compare
      (fun a b -> if a = b then 0 else 1)
      (take 4 (fun () -> cs))
      (String.to_seq "true")
    = 0
  then
    let modl = incc (incc (incc (incc modl))) in
    (spanify modl @@ Literal (Bool true), modl, drop 4 (fun () -> cs) ())
  else word modl cs

and fals modl cs =
  if
    Seq.compare
      (fun a b -> if a = b then 0 else 1)
      (take 5 (fun () -> cs))
      (String.to_seq "false")
    = 0
  then
    let modl = incc (incc (incc (incc (incc modl)))) in
    (spanify modl @@ Literal (Bool false), modl, drop 5 (fun () -> cs) ())
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
        (Nil, incc modl, cs (), true)
    | Cons (':', cs)
      when match cs () with Cons (('\n' | ']' | ';'), _) -> true | _ -> false ->
        (Nil, incc modl, cs (), true)
    | Cons (':', cs) -> prep_a4 (inn (incc modl) (cs ())) Nil
    | Cons ((' ' | '\t'), cs) -> (Nil, incc modl, cs (), false)
    | Cons (('\n' | ']' | ';'), _) as cs -> (Nil, modl, cs, false)
    | Cons (c, cs) -> app_a_inn (inn (incc modl) (cs ())) c
    | cs -> (Nil, incc modl, cs, false)
  in
  inn modl cs |> function
  | Cons (Nil, cs), _, _, _ when cs () = Nil ->
      parse_err modl "Empty Module Configuration" (spanify modl Error)
  | Cons (c, cs), modl, rest, true when cs () = Nil ->
      build ';'
        (function
          | lst, modl ->
              spanify modl
              @@ ModDefine (String.of_seq (fun () -> c), List.rev lst))
        modl
        (fun () -> rest)
  | Cons (c, cs), modl, rest, false when cs () = Nil ->
      (spanify modl @@ Word (String.of_seq (fun () -> c)), modl, rest)
  | cs, modl, rest, _ -> (
      Seq.fold_left
        (fun acc x -> String.of_seq (fun () -> x) :: acc)
        []
        (fun () -> cs)
      |> function
      | w :: ws -> (spanify modl @@ ModCall (ws, w), modl, rest)
      | [] ->
          parse_err modl "Unexpected Word Configuration" (spanify modl Error))

and build ch f modl cs =
  let rec inn acc modl =
    let stream0 f acc cs =
      f modl cs |> fun (tok, modl, cs) -> (tok :: acc, modl, cs) |> uncurry3 inn
    in
    let stream f acc cs =
      f (incc modl) cs |> fun (tok, modl, cs) ->
      (tok :: acc, modl, cs) |> uncurry3 inn
    in
    function
    | Cons (c, cs) when c = ch -> (List.rev acc, incc modl, cs ())
    | Cons ('[', cs) ->
        stream (build ']' (fun (lst, modl) -> spanify modl @@ Quote lst)) acc cs
    | Cons (':', cs) ->
        stream
          (build ';' (function
            | t :: lst, modl ->
                spanify modl @@ Define (string_of_tok t, None, List.rev lst)
            | _, _ ->
                parse_err modl "Bad function definition" (spanify modl Error)))
          acc cs
    | Cons ('(', cs) -> stream comm acc cs
    | Cons ('"', cs) -> stream str acc cs
    | Cons ('t', _) as cs -> stream0 tru acc cs
    | Cons ('f', _) as cs -> stream0 fals acc cs
    | Cons ('0' .. '9', _) as cs -> stream0 int acc cs
    | Cons (' ', cs) -> inn acc (incc modl) (cs ())
    | Cons ('\n', cs) -> inn acc (incl modl) (cs ())
    | Nil ->
        parse_err modl
          ("Expected end character \"" ^ String.make 1 ch ^ "\", not EOF")
          (spanify modl Error)
    | cs -> stream0 word acc cs
  in
  inn [] modl (cs ()) |> fun (lst, modl, cs) -> (f (lst, modl), modl, cs)

let locs : string list = Stdl.Sites.forthish_std

let lookup_file filename dirs =
  List.find_map
    (fun dir ->
      let filename' = Filename.concat dir filename in
      if Sys.file_exists filename' then Some filename' else None)
    dirs

let rec parse (modl : Module.t) =
  let stream0 f cs =
    f modl cs |> fun (tok, modl, cs) -> (eval modl tok, cs) |> uncurry parse
  in
  let stream f cs =
    f (incc modl) cs |> fun (tok, modl, cs) ->
    (eval modl tok, cs) |> uncurry parse
  in
  function
  | Cons ('[', cs) ->
      stream (build ']' (fun (lst, modl) -> spanify modl @@ Quote lst)) cs
  | Cons (':', cs) ->
      stream
        (build ';' (function
          | t :: lst, modl ->
              spanify modl @@ Define (string_of_tok t, None, List.rev lst)
          | _, _ ->
              parse_err modl "Bad function definition" (spanify modl Error)))
        cs
  | Cons ('(', cs) -> stream comm cs
  | Cons ('"', cs) -> stream str cs
  | Cons ('t', _) as cs -> stream0 tru cs
  | Cons ('f', _) as cs -> stream0 fals cs
  | Cons ('0' .. '9', _) as cs -> stream0 int cs
  | Cons (' ', cs) -> parse (incc modl) (cs ())
  | Cons ('\n', cs) -> parse (incl modl) (cs ())
  | Nil -> modl
  | cs -> stream0 word cs

and eval (modl : Module.t) span =
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
  let tok, mname, mline, mcol = span in
  match tok with
  | Literal _ as x -> slide (push x)
  | Quote _ as q -> slide (push q)
  | Comment _ -> modl
  | ModCall (sl, s) ->
      List.rev sl |> List.fold_left find_modl modl |> fun m ->
      let (Module (_, _, _, out, _, _)) = eval m (Word s, mname, mline, mcol) in
      add_out modl out
  | Word w -> (
      match w with
      | w when mem modl w -> (
          find modl w |> eval_list modl |> function
          | Module (_, _, _, out, _, _) -> replace_out modl out)
      | "infile" ->
          match_str
            (fun s ->
              slide (push @@ Literal (Str (open_in s |> In_channel.input_all))))
            modl w span
      | "outfile" ->
          match_str
            (fun s ->
              slide
                (spanify modl (pop modl w span)
                |> string_of_tok
                |> (open_out s |> Out_channel.output_string)))
            modl w span
      | "bind" ->
          match_nonempty_quote
            (fun q ->
              match q with
              | [ (Word n, name, line, col) ] ->
                  add modl
                    ( n,
                      [
                        (Quote [ (Word n, name, line, col) ], mname, mline, mcol);
                      ] )
              | (Word n, _, _, _) :: ts -> add modl (n, ts)
              | a :: _ -> eval_err modl "Expected a Quote of Word and Tokens" a
              | [] ->
                  eval_err modl
                    "Expected a Quote of Word and Tokens Not Nothing"
                    (spanify modl Error))
            modl w span
      | "describe" ->
          slide
            (match pop modl w span with
            | Quote [ (Word n, _, _, _) ] | Literal (Str n) -> (
                match find_opt modl n with
                | Some ts -> print_fun (List.map (fun (a, _, _, _) -> a) ts)
                | None -> print_builtin_fun n modl span)
            | _ -> eval_err modl "Expected a Word or String of a Word" span)
      | "desc" ->
          slide
            (match pop modl w span with
            | Quote [ (Word n, _, _, _) ] | Literal (Str n) -> (
                match find_opt modl n with
                | Some ts -> push @@ Quote ts
                | None -> push @@ Literal (Str n))
            | _ -> eval_err modl "Expected a Word or String of a Word" span)
      | "use" -> match_str (fun s -> eval_file s |> use modl) modl w span
      | "useup" -> match_str (fun s -> eval_file s |> useup modl) modl w span
      | "eval" ->
          match_str (fun s -> parse modl (String.to_seq s ())) modl w span
      | "del" -> match_str (remove modl) modl w span
      | "open" -> match_quote (eval_list modl) modl w span
      | "hd" ->
          match_nonempty_quote (fun q -> List.hd q |> eval modl) modl w span
      | "tl" ->
          match_nonempty_quote
            (fun q -> eval modl (Quote (List.tl q), mname, mline, mcol))
            modl w span
      | "shd" ->
          match_nonempty_quote
            (fun q -> List.rev q |> List.hd |> eval modl)
            modl w span
      | "stl" ->
          match_nonempty_quote
            (fun q -> eval modl (Quote (stl q), mname, mline, mcol))
            modl w span
      | "quote" -> slide (push @@ Quote [ spanify modl @@ pop modl w span ])
      | "+quote" ->
          slide
            (match_quote
               (fun q -> push @@ Quote (spanify modl (pop modl w span) :: q))
               modl w span)
      | "if" ->
          match_quote
            (fun f ->
              match_quote
                (fun t ->
                  match_bool
                    (fun b -> if b then eval_list modl t else eval_list modl f)
                    modl w span)
                modl w span)
            modl w span
      | "dup" -> slide (push @@ top modl w span)
      | "over" ->
          slide
            (let a = pop modl w span in
             let b = top modl w span in
             push a;
             push b)
      | "rot" ->
          slide
            (let a = pop modl w span in
             let b = pop modl w span in
             let c = pop modl w span in
             push b;
             push a;
             push c)
      | "pop" ->
          slide
            (let _ = pop modl w span in
             ())
      | "swap" ->
          slide
            (let a = pop modl w span in
             let b = pop modl w span in
             push a;
             push b)
      | "not" ->
          slide
            (match_bool (fun x -> push @@ Literal (Bool (not x))) modl w span)
      | "and" -> slide (bop ( && ) modl w span)
      | "or" -> slide (bop ( || ) modl w span)
      | "len" -> slide (push @@ Literal (Int (Stack.length stack)))
      | "chars" ->
          match_str
            (fun s ->
              String.to_seq s
              |> Seq.map (fun c -> Literal (Str (String.make 1 c)))
              |> List.of_seq
              |> List.map (fun x -> (x, mname, mline, mcol))
              |> fun x -> Quote x |> push |> slide)
            modl w span
      | "concat" ->
          match_quote
            (fun q ->
              List.map
                (function
                  | Literal (Str s), _, _, _ -> s
                  | x -> eval_err modl "Expected a Quote of Strings or Chars" x)
                q
              |> String.concat ""
              |> fun s -> Literal (Str s) |> push |> slide)
            modl w span
      | "=" ->
          slide
            (isbqop_bool ( = ) ( = ) ( = )
               (fun x y -> List.compare Stdlib.compare x y = 0)
               modl w span)
      | "<>" ->
          slide
            (isbqop_bool ( <> )
               (fun x y -> not (String.equal x y))
               ( <> )
               (fun x y -> List.compare Stdlib.compare x y <> 0)
               modl w span)
      | ">" ->
          slide
            (isop_bool ( > ) (fun x y -> String.compare x y = 1) modl w span)
      | "<" ->
          slide
            (isop_bool ( < ) (fun x y -> String.compare x y = -1) modl w span)
      | "+" -> slide (isqop ( + ) ( ^ ) ( @ ) modl w span)
      | "-" -> slide (iop ( - ) modl w span)
      | "*" -> slide (iop ( * ) modl w span)
      | "**" -> slide (iop pow modl w span)
      | "/" -> slide (iop ( / ) modl w span)
      | "%" -> slide (iop ( mod ) modl w span)
      | "." ->
          add_out modl (spaced (string_of_tok (spanify modl (pop modl w span))))
      | "," -> slide (push @@ Literal (Str (read_line ())))
      | "exit" -> raise Exit
      | _ -> eval_err modl ("Undefined Word \"" ^ w ^ "\"") span)
  | Define (n, _, ts) ->
      List.fold_left
        (fun acc -> function
          | Word "take", name, line, col ->
              (pop modl "take" span, name, line, col) :: acc
          | a, name, line, col -> (a, name, line, col) :: acc)
        [] ts
      |> fun ts -> add modl (n, ts)
  | ModDefine (n, ts) ->
      List.fold_right (fun x y -> eval y x) ts (useup (create n) modl)
      |> use modl
  | Error -> modl

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
      | None -> raise @@ IO_err ("File " ^ filename ^ " not found")
  in
  input |> In_channel.input_all |> fun x ->
  String.to_seq x ()
  |> parse
       (create
          (Filename.basename filename |> Filename.remove_extension
         |> String.capitalize_ascii))
