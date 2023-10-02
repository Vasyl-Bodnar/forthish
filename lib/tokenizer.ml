exception Tok_err of string

type literal = Int of int | String of string | Bool of bool

type token =
  | Literal of literal
  | Word of string
  | Modules of string list
  | Quote of token list
  | Define of string * token list
  | Comment of string

let rec print_token = function
  | Literal (Int x) ->
      print_int x;
      print_string " "
  | Literal (String s) -> print_string ("\"" ^ s ^ "\" ")
  | Literal (Bool b) -> print_string (if b then "true " else "false ")
  | Word w -> print_string (w ^ " ")
  | Modules m -> List.iter (fun m -> print_string (m ^ " ")) m
  | Comment c -> print_string ("( " ^ c ^ " ) ")
  | Quote q ->
      print_string "[ ";
      List.iter print_token q;
      print_string "] "
  | Define (n, ts) ->
      print_string @@ ": " ^ n ^ " ";
      List.iter print_token ts;
      print_string "; "

let to_token = function
  | "true" -> Literal (Bool true)
  | "false" -> Literal (Bool false)
  | str when String.for_all (function '0' .. '9' -> true | _ -> false) str ->
      Literal (Int (int_of_string str))
  | str
    when String.starts_with ~prefix:"\"" str
         && String.ends_with ~suffix:"\"" str ->
      Literal (String (String.sub str 1 (String.length str - 2)))
  | str when String.contains str ':' ->
      String.split_on_char ':' str
      |> List.filter (function "" -> false | _ -> true)
      |> fun x -> Modules x
  | str -> Word str

let rec split_on ?(acc = []) a = function
  | x :: xs when x = a -> (List.rev acc, xs)
  | x :: xs -> split_on ~acc:(x :: acc) a xs
  | _ -> ([], [])

let rec split_on_outer ?(acc = []) ?(n = 0) a b = function
  | x :: xs when x = a -> split_on_outer ~acc:(x :: acc) ~n:(n + 1) a b xs
  | x :: xs when x = b && n = 0 -> (List.rev acc, xs)
  | x :: xs when x = b -> split_on_outer ~acc:(x :: acc) ~n:(n - 1) a b xs
  | x :: xs -> split_on_outer ~acc:(x :: acc) ~n a b xs
  | _ -> ([], [])

let rec to_tokens =
  let comment xs =
    split_on_outer "(" ")" xs |> fun (a, b) ->
    Comment (String.concat " " a) :: to_tokens b
  in
  let quote xs =
    split_on_outer "[" "]" xs |> fun (a, b) ->
    Quote (to_tokens a) :: to_tokens b
  in
  let func name xs =
    split_on_outer ":" ";" xs |> function
    | [], [] -> raise (Tok_err "Expected a proper function definition")
    | a, b -> Define (name, to_tokens a) :: to_tokens b
  in
  function
  | "(" :: xs -> comment xs
  | "[" :: xs -> quote xs
  | ":" :: name :: xs -> func name xs
  | x :: xs -> to_token x :: to_tokens xs
  | _ -> []

let split_space_and_string str =
  let rec prep_l x xs b = function
    | [] -> inner [ Seq.return x ] b xs
    | l :: ls -> inner (Seq.append l (Seq.return x) :: ls) b xs
  and inner acc b xs =
    match xs () with
    | Seq.Cons ('"', xs) when b -> prep_l '"' xs false acc
    | Seq.Cons ('"', xs) -> inner (Seq.return '"' :: acc) true xs
    | (Seq.Cons (' ', xs) | Seq.Cons ('\n', xs)) when not b ->
        inner (Seq.empty :: acc) b xs
    | Seq.Cons (' ', xs) | Seq.Cons ('\n', xs) -> inner acc b xs
    | Seq.Cons (x, xs) -> prep_l x xs b acc
    | Seq.Nil -> acc
  in
  String.to_seq str |> inner [] false
  |> List.filter (fun seq -> match seq () with Seq.Nil -> false | _ -> true)
  |> List.rev_map String.of_seq

let tokenize str = str |> String.trim |> split_space_and_string |> to_tokens
