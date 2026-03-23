# Preclang

Pure Regular Expression Combinator Language (Preclang).

The language comes in two flavours: executable and library.

## The Language

Programs in Preclang are written with pointfree notation,
data is never visible in the program.

### Substitution syntax

Expresions that act as substitution are written as:

```text
s/<input pattern>/<replacement>/<flags>
```

Where:

- `<input pattern>` is a regular expression(-like) to test and match input text
- `<replacement>` is the expression describing how to replace the matched text
- `<flags>` is an optional set of characters changing substitution behavior

The replacement can use syntax such as `\1` or `\{foobar}` to insert group
matches.

Flags include:

- `g` - global, replace all matches
- `i` - insensitive, ignore letter case
- `m` - multiline, do not match line by line
- `u` - unicode support
- `r` - CRLF line-ending support
- `G` - swap greediness behavior
- `S` - ignore whitespace

E.g. `s/foo(.)ar/\1az/gi` will match all occurences of `foo.ar`,
replacing it with the first group followed by `az`,
ignoring letter case.

### Input/output

Any expression (including substitutions) receives text as input and outputs
text + a success flag.

### Operators

Expressions can be combined through operators.

- `&` combines two expressions,
    stopping on the first one that returns a `false` flag,
    applying the output of the first as the input of the second.
- `|` combines two expressions,
    stopping on the first one that returns a `true` flag,
    discarding the output of the first if it fails.
- `;` combines two expressions,
    executing both regardless of success,
    returning the output of the second.
- `!` inverts the output flag of an expression

### Bindings/variables

A let-block declares bindings/variables, to which expressions are assigned.
The syntax is:

```text
let <name0> = <expr0>
let <name1> = <expr1>
...
let <nameN> = <exprN>

in <main expr>
```

A let-block starts with the first `let` and finishes with the expression right
after the nearest following `in`.
The input/output of the expression is given to/extracted from the expresion
right after `in` (the "main" expression).
A let-block creates a scope according to the following
rules:

- All bindings of the same let-block are visible to each other
    regardles of order
    - They can be recursive with each other
- Scopes are nested, inner blocks can access outer blocks, but not the opposite

## Examples

### Recursive unary numbers addition

Let's represent numbers with repeated `1`s, repeating them as much as the
number counts.
Let's separate numbers with a colon `:`.
The following program adds two numbers:

```text
let case_zero = s//
let case_succ = s/^1(1*):/\1:1/
let induction = (case_succ & induction) | case_zero
in induction
```

## Library

Preclang is available as a library of the Rust programming language.
You can check the documentation with:

```sh
cargo doc --open
```

The library provides the ability of limiting execution time or memory usage.
For instance, parsing depth can be limited.
Execution by AST can have its depth limited.
Execution by bytecode can have total cycles limited and call stack limited.

## Executable

To compile the interpreter,
you will need tooling for the Rust programming language.

To compile it, run:

```sh
cargo run --release -p preclang-bin
```

Then extract the binary from:

```
./target/release/preclang-bin
```

Check usage by calling the program with `--help`:

```sh
./preclang-bin --help
```

Example output:

```text
Executes programs of the Pure Regular Expression Combinator Language (Preclang).

By default, source code is provided through a file, whose path is provided as a positional argument, and input text is provided through the standard input.

Usage: preclang-bin [OPTIONS] [PROGRAM_PATH]

Arguments:
  [PROGRAM_PATH]
          Path to the file containing the source code.
          
          Accepted and required only if no other means of providing source code is specified.

Options:
  -p, --program <CODE>
          Inline source code.
          
          If provided, positional source code path will not be accepted. Standard input will still be available to input text.

      --stdin-program
          Uses standard input to receive source code.
          
          If provided, neither positional source code path nor inline source code will be accepted. Standard input will not be available to input text and alternative means must be used to provide it.

  -i, --input <INPUT>
          Inline input text.
          
          If provided, standard input will not be used to receive input text, and path to file will also not be accepted as means to provide it.

  -f, --file-input <INPUT_PATH>
          Path to file containing input text.
          
          If provided, standard input will not be used to receive input text, and the inline argument will also not be accepted as means to provide it.

  -I, --interpreter <INTERPRETER>
          Which interpreter method to use.
          
          ast - interpret by directly walking through the AST.
          
          bytecode - interpret by first compiling the AST to bytecode and then executing the bytecode.
          
          [default: ast]
          [possible values: ast, bytecode]

  -S, --emit-asm
          Emits textual, assembly-like representation of bytecode.
          
          If provided, input text will not be accepted and the programm will not be  executed.

  -x, --expand-subs
          Expands substitution tokens in textual representation of byte.
          
          This requires assembly emission mode. By default, emitting assembly will print numeric identifiers of substitution; this option enables expanding it directly into instructions instead.

  -h, --help
          Print help (see a summary with '-h')
```
