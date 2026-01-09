# A mini LucidSynchrone compiler

LucidSynchrone is a synchrone programming language based on OCaml. Our compiler support a subset/variant of it and
outputs OCaml code. A big effort was make to produce good error messages and support many features. However, it is only a student project done in a very small time window alone.

Example of LucidSynchrone code:
```ocaml
(* Derivative! *)
let node deriv x dt = y
  where y = (x -. 0 fby x) ./ dt

(* Type sums are supported *)
type state = S0 | S1 | S2

(* Code that does nothing... *)
let node g x (y : s) z =
  let z = x when S0(x) in
  let w = (z == z) in
  0 + pre y
```

You can also use OCaml code directly:
```ocaml
type my_integer = int

let f (x : my_integer) = x + 4
```

The compiler also support a auto-completion feature. It can be triggered with:
```bash
compiler input.sync --autocomplete line:col
```
where `line` and `col` are the 1-based line and column numbers of the cursor where suggestions should be generated.

## Building

The project uses the Dune build system. You can call:
```bash
dune build
# Or execute directly the compiler with:
dune exec ./bin/main.exe -- input.sync
```
You can also use the Makefile (which use Dune behind):
```bash
make build
# Or to execute the compiler on input.sync
make input_tast # Dumps the Typed AST
make input_nast # Dumps the Normalized AST
make input_ir   # Dumps the intermediate imperative representation
make input      # Generates OCaml code to input.ml and calls the OCaml compiler
```

## Options

The compiler takes the following options:
```
Usage: minisync [options] <filename>
  -i                     Print the interface of the checked program.
  --check                Only typecheck the input program.
  --autocomplete         <line>:<column> Perform autocomplete at the given location.
  --color {always|never|auto}
                         Set if colors are used in output.
  --error-format {ocaml|gcc}
                         Set the error message format.
  --error-lines          Show source code lines in error messages.
  --no-error-lines       Do not show source code lines in error messages.
  --no-warnings          Do not show warnings.
  --warnings-as-errors   Treat warnings as errors.
  --dump-tokens          Print the list of tokens produced by the lexer.
  --dump-tast            Print the typed abstract syntax tree (TAST) of the checked program.
  --dump-nast            Print the normalized abstract syntax tree (NAST) of the checked program.
  --dump-ir              Print the intermediate representation (IR) of the compiled program.
  --print-types          Print types in the dumps (tast, nast, ir, ...).
  --print-clocks         Print clocks in the dumps (tast, nast, ir, ...).
  --print-locations      Print source code locations in the dumps (tast, nast, ir, ...).
  --print-var-types-ids  When dumping the TAST, NAST or IR, also print the unique ids of variable types/clocks.
  --optimize-ir          Enable or disable IR optimizations.
  -help                  Display this list of options
  --help                 Display this list of options
```

## Tests

A few integration tests are provided in the `test` directory. You can execute them with
```bash
make test
```

You can also run unit tests in the OCaml code with:
```bash
dune test
```
