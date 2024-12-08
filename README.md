# forthish

Objective Caml interpreter for my custom Forthish language.

## Details

Forthish is a stack-based, functional concatenative programming language inspired by Forth and Factor.

### Language

- Words are defined using the `: ;` notation from Forth, so `: fun 1 1 + ;` defines fun to putting two 1s and adding them.
- Alternatively `bind` can be used to bind a word to a quotation, so `[ fun 1 1 + ] bind` is equivalent to the above.
  Note that that both of them are lexically scoped. If you want to bind a word in outer scope, you will have to return a quote and open it.
- You can define variables inside functions using a powerful `take` word. 
  For example `: x take ;` will 'take' the variable on top of the stack and associate it with x at runtime.
- Modules can be defined using the same `: ;`, as in `Module: ( Words and Definitions ) ;`. 
  This will define a module with the given name and the words inside it and bring it into scope.
- Quotations are enclosed in square brackets `[ ]` like in Factor. 
  They are quite powerful, and can be used for anonymious functions and laziness, or Lisp style.
- Literals can be Integers (-35 or 21), Strings (`"Hello"`), or Boolean values (`true` or `false`).
- Comments are enclosed in parentheses `( )` as is traditional in Forth.

### Built-in Words

Certain words are already predefined as core for various uses:

- Stack manipulation: `dup`, `pop`, `swap`, `over`, `rot`
- Control flow: `if`, `open` (opens quotes)
- List operations: `hd`, `tl`, `shd`, `stl`, `chars`, `concat`, `quote`, `+quote`
- Arithmetic operations: `+`, `-`, `*`, `**`, `/`, `%`
- Comparison operations: `=`, `<>`, `>`, `<`
- Boolean operations: `and`, `or`, `not`
- I/O operations: `.` (print top), `,` (read input line) 
- File: `infile` (read entire file), `outfile` (write string to file)
- Modules: `use` (bring module into scope), `useup` (merge module into current scope)
- Meta: `describe` (definitions), `len` (stack size), 
        `eval` (arbitrary code execution), `del` (remove definition), 
        `bind`, `take`, `exit`

### Modules

Files are considered modules, and can be imported using the `use` or `useup` word. 

For example, `"file.ftsh" use` will import the file level module in `file.ftsh` from same directory as `File`. 

Alternatively, `"file.ftsh" useup` will merge the file level module into the current scope, so that the words are available directly.

You can also define modules directly using the `SomeModule: ( Some Words and Definitions ) ;` syntax. 
It would be evaluated the same way as a file and brought into scope in the way of `use`.

Currently there are two modules available in the standard library: 
`list` and `util`, that provide many list operations and a couple of utility functions.
These can be imported in the same way as above files, but if using `dune install`, 
these should be preinstalled in a place that the interpreter can find them from anywhere.

### Examples

Here are some simple examples of Forthish code:

```forth
"list" use
( Usual function implementations )

: fact 1 swap List:.. List:prod ;
: sum [ + ] List:rreduce ;
: fib dup 2 > [ dup 1 - fib swap 2 - fib + ] [ pop 1 ] if ;

5 fact .
[ 5 6 7 8 ] sum .
8 fib .
(Outputs:120 26 21)
```

```forth
"list" useup
( Modules example )

Ingrdt: [ Onion Oil Salt Pepper ] enum! open ;

Recipe: 
  : nl "\n" . ;
  : drizzle Ingrdt:Oil = [ "Drizzle Oil" . ] [] if ;
  : chop desc "Chop " swap + . ;
  : fry "Fry it for" . . "seconds" . ;
  : add [
      ["Salt to taste" [Ingrdt:Salt =]]
      ["Grind some Pepper" [Ingrdt:Pepper =]]
      ["Add Onion" [Ingrdt:Onion =]]
    ] matchw open . ; 
  : finish "Finished" . ; ;

: nl Recipe:nl ;

Ingrdt:Oil Recipe:drizzle nl
Ingrdt:Onion Recipe:chop nl
Ingrdt:Onion Recipe:add nl
180 Recipe:fry nl
Ingrdt:Salt Recipe:add nl
Ingrdt:Pepper Recipe:add nl
Recipe:finish
;
```

### Building

There are three components here, the REPL and file interpreter in `bin/`, the parser and core interpreter in `lib/`, and web interface in `web/`.

You can build them using dune, simply by running `dune build` for binary and `dune build @default` for the web interface.

There are dependencies on `js_of_ocaml` and `ocamline`, so you may need to install them with `opam`.

### Usage

`forthish` binary has a few modes:
- `forthish` will start the REPL environment.
- `forthish file.fthish` will interpret the file.
- `forthish -e "code"` will interpret the code string.
- `forthish -i <file> arg1 arg2` will run the interpreter on a file and provide bash style arguments ($0 is the file name string, $1 is the first argument string, etc).
- `forthish [-]-help` will show the help message.

The web interface can be run locally in the `_build/default/web` directory using `python3 -m http.server` or similar.
The web interface is a simple REPL that can be used to run Forthish code in the browser.
It automatically imports the standard library modules, so you can use them directly.
