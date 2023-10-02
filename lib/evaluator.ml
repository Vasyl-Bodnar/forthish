open Tokenizer

module Module = struct
  type def = string * token list
  type t = Module of string * def list * t list

  let create name = Module (name, [], [])

  let useup (Module (name_a, lst_a, modls_a)) (Module (_, lst_b, modls_b)) =
    Module (name_a, lst_a @ lst_b, modls_a @ modls_b)

  let use (Module (name, lst, modls)) modl = Module (name, lst, modl :: modls)
  let add (Module (name, lst, modls)) def = Module (name, def :: lst, modls)

  let remove (Module (name, lst, modls)) def_name =
    Module (name, List.remove_assoc def_name lst, modls)

  let find (Module (_, lst, _)) def_name = List.assoc def_name lst

  let find_mod (Module (_, _, modls)) modl_name =
    List.find (fun (Module (name, _, _)) -> name = modl_name) modls

  let mem (Module (_, lst, _)) def_name = List.mem_assoc def_name lst
  (* let print_def (str, ts) = print_string (str ^ " "); List.iter print_token ts
     let rec print_module (Module (name, lst, modls)) = print_string (name ^ " "); List.iter print_def lst; List.iter print_module modls *)
end

include Module

let stack : token Stack.t = Stack.create ()
let push x = Stack.push x stack
let pop () = Stack.pop stack
let top () = Stack.top stack

exception Eval_err of string

let err a s =
  push a;
  raise (Eval_err ("Err: " ^ s))

let match_bool f =
  match pop () with
  | Literal (Bool b) -> f b
  | a -> err a "Expected a Bool Argument"

let match_int f =
  match pop () with
  | Literal (Int x) -> f x
  | a -> err a "Expected an Integer Argument"

let match_str f =
  match pop () with
  | Literal (String s) -> f s
  | a -> err a "Expected a String Argument"

let match_quote f =
  match pop () with Quote q -> f q | a -> err a "Expected a Quote Argument"

let bop f =
  match_bool (fun y -> match_bool (fun x -> push @@ Literal (Bool (f x y))))

let iop f =
  match_int (fun y -> match_int (fun x -> push @@ Literal (Int (f x y))))

let isqop f g h =
  match pop () with
  | Literal (Int y) -> match_int (fun x -> push @@ Literal (Int (f x y)))
  | Literal (String d) -> match_str (fun s -> push @@ Literal (String (g s d)))
  | Quote q -> match_quote (fun e -> push @@ Quote (h e q))
  | a -> err a "Expected either Integers or Strings"

let isop_bool f g =
  match pop () with
  | Literal (Int y) -> match_int (fun x -> push @@ Literal (Bool (f x y)))
  | Literal (String d) -> match_str (fun s -> push @@ Literal (Bool (g s d)))
  | a -> err a "Expected either Integers or Strings"

let isbop_bool f g h =
  match pop () with
  | Literal (Int y) -> match_int (fun x -> push @@ Literal (Bool (f x y)))
  | Literal (String d) -> match_str (fun s -> push @@ Literal (Bool (g s d)))
  | Literal (Bool n) -> match_bool (fun b -> push @@ Literal (Bool (h b n)))
  | a -> err a "Expected either Integers, Strings or Bools"

let rec eval modl =
  let slide f lst =
    f;
    eval modl lst
  in
  function
  | (Literal _ as x) :: lst -> slide (push x) lst
  | (Quote _ as q) :: lst -> slide (push q) lst
  | Modules xs :: lst ->
      let rec inn modl = function
        | [ x ] -> (find modl x, modl)
        | x :: xs -> inn (find_mod modl x) xs
        | [] -> raise (Eval_err "Expected a Module")
      in
      inn modl xs |> fun (l, m) ->
      eval m l |> fun _ -> eval modl lst
  | Comment _ :: lst -> eval modl lst
  | Word w :: lst -> (
      match w with
      | w when mem modl w -> find modl w |> eval modl |> fun _ -> eval modl lst
      | "use" ->
          (match_str (fun s -> eval_file (s ^ ".4ish")) |> use modl |> eval) lst
      | "useup" ->
          (match_str (fun s -> eval_file (s ^ ".4ish")) |> useup modl |> eval)
            lst
      | "eval" ->
          match_str (fun s -> tokenize s |> fun l -> eval modl (l @ lst))
      | "del" -> (match_str @@ remove modl |> eval) lst
      | "open" -> match_quote @@ fun l -> eval modl (l @ lst)
      | "if" ->
          match_quote (fun f ->
              match_quote (fun t ->
                  match_bool (fun b ->
                      if b then eval modl (t @ lst) else eval modl (f @ lst))))
      | "quote" -> slide (push @@ Quote [ pop () ]) lst
      | "dup" -> slide (push @@ top ()) lst
      | "pop" ->
          slide
            (let _ = pop () in
             ())
            lst
      | "swap" ->
          slide
            (let a = pop () in
             let b = pop () in
             push a;
             push b)
            lst
      | "not" ->
          slide (match_bool (fun x -> push @@ Literal (Bool (not x)))) lst
      | "len" -> slide (push @@ Literal (Int (Stack.length stack))) lst
      | "and" -> slide (bop ( && )) lst
      | "or" -> slide (bop ( || )) lst
      | "=" -> slide (isbop_bool ( = ) ( = ) ( = )) lst
      | "<>" ->
          slide
            (isbop_bool ( <> ) (fun x y -> not (String.equal x y)) ( <> ))
            lst
      | ">" -> slide (isop_bool ( > ) (fun x y -> String.compare x y = 1)) lst
      | "<" -> slide (isop_bool ( < ) (fun x y -> String.compare x y = -1)) lst
      | "+" -> slide (isqop ( + ) ( ^ ) ( @ )) lst
      | "-" -> slide (iop ( - )) lst
      | "*" -> slide (iop ( * )) lst
      | "/" -> slide (iop ( / )) lst
      | "%" -> slide (iop ( mod )) lst
      | "." -> slide (pop () |> print_token) lst
      | "," -> slide (push @@ Literal (String (read_line ()))) lst
      | "exit" -> raise Exit
      | _ -> raise (Eval_err ("Err: Undefined Word " ^ w)))
  | Define (n, ts) :: lst ->
      List.rev ts
      |> List.fold_left
           (fun acc -> function Word "take" -> pop () :: acc | a -> a :: acc)
           []
      |> fun ts ->
      add modl (n, ts) |> fun x -> eval x lst
  | [] -> modl

and eval_file filename =
  open_in filename |> In_channel.input_all |> tokenize
  |> eval
       (create
          (String.split_on_char '/' filename
          |> List.rev |> List.hd |> String.split_on_char '.' |> List.hd
          |> String.capitalize_ascii))

(*
and info lst =
  List.map (function
      | Literal (Int _) -> Type Int
      | Literal (String _) -> Type String
      | Literal (Bool _) -> Type Bool
      | Quote _ -> Type Quote
      | Comment _ -> Type Unit
      | Word w -> (match w with
          | w when Hashtbl.find_opt defs w |> Option.is_some -> Hashtbl.find defs w |> fun (t, _) -> t
          | "eval" -> Function ([String], Nil)
          | "open" -> Function ([Quote], Nil)
          | "quote" -> Function ([Generic 1], Quote)
          | "dup" -> Function ([Generic 1], Multiple [Generic 1; Generic 1])
          | "pop" -> Function ([Generic 1], Unit)
          | "swap" -> Function ([Generic 1; Generic 2], Multiple [Generic 2; Generic 1])
          | "not" -> Function ([Bool],Bool)
          | "if" -> Function ([Bool; Quote; Quote], Nil)
          | "len" -> Function ([],Int)
          | "del" -> Function ([String],Unit)
          | "and" -> Function ([Bool; Bool],Bool)
          | "or" -> Function ([Bool; Bool],Bool)
          | "=" -> Function ([Or [Int; String; Bool]; Or [Int; String; Bool]],Bool)
          | "<>" -> Function ([Or [Int; String; Bool]; Or [Int; String; Bool]],Bool)
          | ">" -> Function ([Or [Int; String]; Or [Int; String]],Bool)
          | "<" -> Function ([Or [Int; String]; Or [Int; String]],Bool)
          | "+" -> Function ([Or [Int; String; Quote]; Or [Int; String; Quote]], Or [Int; String; Quote])
          | "-" -> Function ([Int; Int], Int)
          | "*" -> Function ([Int; Int], Int)
          | "/" -> Function ([Int; Int], Int)
          | "%" -> Function ([Int; Int], Int)
          | "." -> Function ([Generic 1], Unit)
          | "," -> Function ([], String)
          | _ -> raise (Eval_err ("Err: Undefined Word " ^ w))
        )
      | Define(_, ts) -> info ts)
*)
