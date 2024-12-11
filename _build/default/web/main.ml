open Js_of_ocaml
module Html = Dom_html
open Forthish.Parser

let pop_output = function
  | Module (name, fns, modls, out, i, j) ->
      (out, Module (name, fns, modls, "", i, j))

let run modl str =
  try
    String.trim str |> fun s -> (String.to_seq s) () |> parse modl |> pop_output
  with
  | IO_err s -> (s, modl)
  | Eval_err (s, _, _) -> (s, modl)
  | Parse_err (s, _, _) -> (s, modl)
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

let std_lib : string =
  {|
: id ; ( -> )

: quote+ quote + ;

: succ 1 + ;

: pred 1 - ;

: rott rot rot ; ( a b c -> c a b )

: dup2 over over ; ( a b -> a b a b )

: <= dup2 = rrot < or ; ( =a a -> a )

: int->bool 0 > ; ( i -> ? )

: bool->int [1] [0] if ; ( ? -> i )

: mapq
  : sshd 
    dup tl [] =
    [] [ tl sshd ] if ;
  : inn 
    dup tl [] <>
    [ dup stl inn swap sshd quote+ ]
    [ quote ]
    if ;
  inn ;

: dotimes ( q i -> ! )
  : I take ;
  : F take open ;
  : inn
    I 0 >
      [ F I 1 - : I take ; inn ]
      [ ]
    if ;
  inn ;

: dump [ . ] len 1 - dotimes ;

: .. ( i i -> qi )
  : I take 1 - ;
  [] +quote [ dup hd succ swap +quote ] I dotimes ;

: repeat ( a i -> qa )
  : I take ;
  : A take ;
  [] [ A swap +quote ] I dotimes ;

: interleave (qa b -> q[a b a..])
  : V take ;
  : inn 
    dup [] <>
    [ dup hd : H take ; tl inn H quote+ V + ]
    [ ]
    if ;
  inn ;

: fold ( qa qb :qb a -> qb; -> qb )
  : F take open ;
  : inn
    swap dup [] <>
      [ swap over shd F swap stl swap inn ]
      [ pop ]
    if ;
  inn ;

: reduce ( qa :qa a -> qa; -> qa )
  : F take open ;
  : inn
    swap dup [] <>
      [ swap over shd F swap stl swap inn ]
      [ pop ]
    if ;
  dup shd swap stl swap inn ;

: rfold ( qa qb :qb a -> qb; -> qb )
  : F take open ;
  : inn
    swap dup [] <>
      [ swap over hd F swap tl swap inn ]
      [ pop ]
    if ;
  inn ;

: rreduce ( qa :qa a -> qa; -> qa )
  : F take open ;
  : inn
    swap dup [] <>
      [ swap over hd F swap tl swap inn ]
      [ pop ]
    if ;
  dup hd swap tl swap inn ;

: mem ( qa a -> ? )
  [ = [ pop true ] [] if ] +quote false swap rfold ;

: any ( qa :a -> ?; -> ?)
  [ [ pop true ] [] if ] + false swap rfold ;

: all ( qa :a -> ?; -> ?)
  [ [] [ pop false ] if ] + true swap rfold ;

: filter (qa :a -> ?; -> qa)
  [ dup ] swap + [ [ swap +quote ] [ pop ] if ] + [] swap fold ;

: rfilter (qa :a -> ?; -> qa)
  [ dup ] swap + [ [ swap +quote ] [ pop ] if ] + [] swap rfold ;

: count (q -> i)
  0 [ pop 1 + ] rfold ;

: rev ( qa -> qa )
  [] [ swap +quote ] rfold ;

: map ( qa :a -> b; -> qb )
  [ swap +quote ] + [] swap fold ;

: rmap ( qa :a -> b; -> qb )
  [ swap +quote ] + [] swap rfold ;

: iter ( qa :a -> u; -> u )
  [] swap fold ;

: sum ( qi -> i )
  [ + ] rreduce ;

: prod ( qi -> i )
  [ * ] rreduce ;

: max ( =qa -> a )
  [ dup2 > [ pop ] [ swap pop ] if ] rreduce ;

: min ( =qa -> a )
  [ dup2 < [ pop ] [ swap pop ] if ] rreduce ;

: enum!
  mapq [ [ bind ] +quote ] rmap [ open ] interleave ;

: match ( [ a [?] ] -> a )
  [ ] [ open open [ swap +quote ] [ pop [] + ] if ] fold ;

: matchw ( a [ b :a -> ?; ] -> b )
  swap : A take ; [ ] [ open A swap open [ swap +quote ] [ pop [] + ] if ] fold ;

: digits (i -> q)
  : TL [] ;
  : inn
    dup 0 >
      [ dup 10 % TL +quote : TL take ; 10 / inn ]
      [ pop TL ]
    if ;
  dup 10 % TL +quote : TL take ; 10 / inn ;

: int->str (i -> s)
  digits
  [[ ["0" [0 = ]]
     ["1" [1 = ]]
     ["2" [2 = ]]
     ["3" [3 = ]]
     ["4" [4 = ]]
     ["5" [5 = ]]
     ["6" [6 = ]]
     ["7" [7 = ]]
     ["8" [8 = ]]
     ["9" [9 = ]]
   ] matchw open
  ] map concat ;

: str->int (s -> i)
  chars
  [[ [0 ["0" = ]]
     [1 ["1" = ]]
     [2 ["2" = ]]
     [3 ["3" = ]]
     [4 ["4" = ]]
     [5 ["5" = ]]
     [6 ["6" = ]]
     [7 ["7" = ]]
     [8 ["8" = ]]
     [9 ["9" = ]]
   ] matchw open
  ] rmap 0 [ 10 * swap 10 * + ] fold 10 / ;|}

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
  let modl = ref (create "") in
  let _, m = run !modl std_lib in
  modl := m;
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
