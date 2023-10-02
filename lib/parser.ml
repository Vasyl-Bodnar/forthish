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
  | ModCall of string list * string
  | Comment of string

let rec string_of_tok = function
  | Literal (Int i) -> string_of_int i
  | Literal (Str s) -> s
  | Literal (Bool b) -> string_of_bool b
  | Word w -> w
  | Quote q -> List.rev_map string_of_tok q |> List.fold_left (fun acc x -> acc ^ " " ^ x) "[" |> fun s -> s ^ " ]"
  | Define (n, _, ts) ->
      n ^ (List.rev_map string_of_tok ts |> List.fold_left (fun acc x -> acc ^ " " ^ x) ":")
  | ModCall (sl, s) -> sl @ [ s ] |> String.concat ":"
  | Comment c -> c

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
  | ModCall (sl, s) ->
      sl @ [ s ] |> String.concat ":" |> print_string;
      print_string ""
  | Comment c ->
      print_string "( ";
      print_string c;
      print_string " )"

open Seq

exception Parse_err of string * int
exception Eval_err of string * int

module Module = struct
  type fdef = string * token list
  type t = Module of string * fdef list * t list * string * int

  let create name = Module (name, [], [], "", 0)

  let useup (Module (name_a, lst_a, modls_a, out, i))
      (Module (_, lst_b, modls_b, modout, j)) =
    Module (name_a, lst_b @ lst_a, modls_b @ modls_a, modout ^ out, i + j)

  let use (Module (name, lst, modls, out, i)) modl =
    Module (name, lst, modl :: modls, out, i)

  let add (Module (name, lst, modls, out, i)) def =
    Module (name, def :: lst, modls, out, i)

  let remove (Module (name, lst, modls, out, i)) def_name =
    Module (name, List.remove_assoc def_name lst, modls, out, i)

  let find (Module (_, lst, _, _, _)) def_name = List.assoc def_name lst

  let find_modl (Module (_, _, modls, _, _)) modl_name =
    List.find (fun (Module (name, _, _, _, _)) -> name = modl_name) modls

  let mem (Module (_, lst, _, _, _)) def_name = List.mem_assoc def_name lst

  let incl (Module (name, lst, modls, out, i)) =
    Module (name, lst, modls, out, succ i)

  let add_out (Module (name, lst, modls, out, i)) print =
    Module (name, lst, modls, out ^ print, succ i)

  let replace_out (Module (name, lst, modls, _, i)) print =
    Module (name, lst, modls, print, succ i)

  let eval_err (Module (_, _, _, _, i)) s = raise (Eval_err (s, i))
  let parse_err (Module (_, _, _, _, i)) s = raise (Parse_err (s, i))
  (* let print_def (str, ts) = print_string (str ^ " "); List.iter print_token ts
     let rec print_module (Module (name, lst, modls)) = print_string (name ^ " "); List.iter print_def lst; List.iter print_module modls *)
end

include Module

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

let match_real_quote f modl name =
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
let map_a f (a, b, c) = (f a, b, c)
let prep_a (a, b, c) d = (Cons (d, fun () -> a), b, c)

let wrd modl cs =
  let app_a_inn (a, b, c) d =
    ( (match a with
      | Cons (c, cs) -> Cons (Cons (d, fun () -> c), cs)
      | Nil -> Cons (Cons (d, empty), empty)),
      b,
      c )
  in
  let rec inn modl = function
    | Cons (':', cs) -> prep_a (inn modl (cs ())) Nil
    | Cons (' ', cs) -> (Nil, modl, cs ())
    | Cons ('\n', cs) -> (Nil, incl modl, cs ())
    | Cons (c, cs) -> app_a_inn (inn modl (cs ())) c
    | cs -> (Nil, modl, cs)
  in
  inn modl cs
  |> map_a (function
       | Cons (c, cs) when cs () = Nil -> Word (String.of_seq (fun () -> c))
       | Cons (_, _) as cs -> (
           Seq.fold_left
             (fun acc x -> String.of_seq (fun () -> x) :: acc)
             []
             (fun () -> cs)
           |> function
           | w :: ws -> ModCall (ws, w)
           | [] -> parse_err modl "Unexpected Word Configuration")
       | Nil -> parse_err modl "Impossible Word Configuration")

let int modl cs =
  let rec inn modl = function
    | Cons (('0' .. '9' as i), cs) -> prep_a (inn modl (cs ())) i
    | Cons (' ', cs) -> (Nil, modl, cs ())
    | Cons ('\n', cs) -> (Nil, incl modl, cs ())
    | cs -> (Nil, modl, cs)
  in
  inn modl cs
  |> map_a (fun seq ->
         Literal
           (Int
              (String.of_seq (fun () -> seq) |> int_of_string_opt |> function
               | Some v -> v
               | None -> parse_err modl "Number is too large")))

(* TODO: Improve this to a proper solution *)
let tru modl cs =
  if
    Seq.compare
      (fun a b -> if a = b then 0 else 1)
      (take 4 (fun () -> cs))
      (String.to_seq "true")
    = 0
  then (Literal (Bool true), modl, drop 4 (fun () -> cs) ())
  else wrd modl cs

let fals modl cs =
  if
    Seq.compare
      (fun a b -> if a = b then 0 else 1)
      (take 5 (fun () -> cs))
      (String.to_seq "false")
    = 0
  then (Literal (Bool false), modl, drop 5 (fun () -> cs) ())
  else wrd modl cs

let comm modl cs =
  let rec inn acc modl = function
    | Cons ('(', cs) -> inn acc modl (cs ()) |> uncurry3 inn
    | Cons (')', cs) -> (List.rev acc, modl, cs ())
    | Cons ('\n', cs) -> inn acc (incl modl) (cs ())
    | Cons (c, cs) -> inn (c :: acc) modl (cs ())
    | Nil -> parse_err modl "Expected end of comment, not EOF"
  in
  inn [] modl (cs ())
  |> map_a (fun seq -> Comment (List.to_seq seq |> String.of_seq))

let str modl cs =
  let rec inn acc modl = function
    | Cons ('\\', cs) -> (
        match cs () with
        | Cons ('"', cs) -> inn ('"' :: acc) modl (cs ())
        | _ -> inn ('\\' :: acc) modl (cs ()))
    | Cons ('"', cs) -> (List.rev acc, modl, cs ())
    | Cons ('\n', cs) -> inn acc (incl modl) (cs ())
    | Cons (c, cs) -> inn (c :: acc) modl (cs ())
    | Nil -> parse_err modl "Expected end of string, not EOF"
  in
  inn [] modl (cs ())
  |> map_a (fun seq -> Literal (Str (List.to_seq seq |> String.of_seq)))

let rec build ch f modl cs =
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
    | cs -> stream wrd acc cs
  in
  inn [] modl (cs ()) |> map_a f

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
  | cs -> stream wrd cs

and eval (modl : Module.t) =
  let slide f =
    f;
    modl
  in
  let eval_list modl = List.fold_left eval modl in
  let rec stl = function _ :: [] | [] -> [] | x :: xs -> x :: stl xs in
  function
  | Literal _ as x -> slide (push x)
  | Quote _ as q -> slide (push q)
  | Comment _ -> modl
  | ModCall (sl, s) ->
      List.rev sl |> List.fold_left find_modl modl |> fun m ->
      let _ = eval m (Word s) in
      modl
  | Word w -> (
      match w with
      | w when mem modl w -> (
          find modl w |> eval_list modl |> function
          | Module (_, _, _, out, _) -> replace_out modl out)
      | "use" -> match_str (fun s -> eval_file s |> use modl) modl w
      | "useup" -> match_str (fun s -> eval_file s |> useup modl) modl w
      | "eval" -> match_str (fun s -> parse modl (String.to_seq s ())) modl w
      | "del" -> match_str (remove modl) modl w
      | "open" -> match_quote (eval_list modl) modl w
      | "hd" -> match_real_quote (fun q -> List.hd q |> eval modl) modl w
      | "tl" -> match_real_quote (fun q -> eval modl (Quote (List.tl q))) modl w
      | "shd" ->
          match_real_quote (fun q -> List.rev q |> List.hd |> eval modl) modl w
      | "stl" -> match_real_quote (fun q -> eval modl (Quote (stl q))) modl w
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
      | "len" -> slide (push @@ Literal (Int (Stack.length stack)))
      | "and" -> slide (bop ( && ) modl w)
      | "or" -> slide (bop ( || ) modl w)
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
      | "." -> add_out modl (string_of_tok (pop modl w) ^ " ")
      | "," -> slide (push @@ Literal (Str (read_line ())))
      | "exit" -> raise Exit
      | _ -> eval_err modl @@ "Undefined Word \"" ^ w ^ "\"")
  | Define (n, Some _, ts) ->
      List.fold_left
        (fun acc -> function
          | Word "take" -> pop modl "take" :: acc
          | a -> a :: acc)
        [] ts
      |> fun ts -> add modl (n, ts)
  | Define (n, None, ts) ->
      List.fold_left
        (fun acc -> function
          | Word "take" -> pop modl "take" :: acc
          | a -> a :: acc)
        [] ts
      |> fun ts -> add modl (n, ts)

and eval_file filename =
  (if String.ends_with ~suffix:".4ish" filename then open_in filename
   else open_in (filename ^ ".4ish"))
  |> In_channel.input_all
  |> fun x ->
  String.to_seq x ()
  |> parse
       (create
          (String.split_on_char '/' filename
          |> List.rev |> List.hd |> String.split_on_char '.' |> List.hd
          |> String.capitalize_ascii))
