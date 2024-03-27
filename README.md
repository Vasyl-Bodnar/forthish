# forthish

Objective Caml Interpreter for my custom Forthish language.

## Details

Forthish is a stack-based, concatenative programming language inspired by Forth. It is also inspired by more functional features of Factor such as quoting.

### Syntax
- Words are defined using the `: ;` notation from Forth, so `: fun 1 1 +;` defines fun to putting two 1s and adding them.
- You can define variables inside functions using a powerful `take` word. For example `: x take ;` will 'take' the variable on top of the stack and associate it with x at runtime.
- Quotations are enclosed in square brackets `[ ]` like in Factor. They are quite powerful, and can be used for anonymious functions and laziness, or Lisp style.
- Literals can be Integers (-35 or 21), Strings (`"Hello"`), or Boolean values (`true` or `false`).
- Comments are enclosed in parentheses `( )` as is traditional in Forth.

### Built-in Words
Certain words are already predefined for various uses:

- Stack manipulation: `dup`, `pop`, `swap`, `over`, `rot`
- Control flow: `if`, `quote`, `+quote`, `open`
- List operations: `hd`, `tl`, `shd`, `stl`, `chars`, `concat`
- Arithmetic operations: `+`, `-`, `*`, `**`, `/`, `%`
- Comparison operations: `=`, `<>`, `>`, `<`
- Boolean operations: `and`, `or`, `not`
- Meta: `len`, `.` (print), `,` (read input), `exit`

### Modules
`use` and `useup` words can be used to import other script files and their definitions like modules.
There is a list.4ish file in std folder which implements many common list operations like map as an example.
This system is to be further expanded.
