# SYSU Compiler Princple Homework

--- of making a compiler for some much simplified c-like language.

The Language is so-called "C minus minus" language, most of the test cases are alike to the ones of NJU Compiler Principle Course.

It's syntax is so limited that I have to rewrite a new lexer and parser in just about 6 hours in the first day to complete this report.

Can count this as a speedrun, satisfied.

---

Added options to just dump lexical tokens only, and to dump ast in m-expr, run with `--help` to see these options.

---

This project is done under OCaml, the ultimate answer for compiler principle and any text processing problems, famous for Coq.

you can build the main program with command:

```sh
dune build
```

The build product is `_build/default/bin/main.exe`, though suffixed with `.exe`, it is actually an `ELF 64-bit LSB pie executable, x86-64, for GNU/Linux 4.4.0, with debug_info, not stripped` on my `Arch Linux x86_64`.

Of course, you can build and run it directly in one command:

```sh
dune exec cph <filename>
```

where `<filename>` stands for your `.cmm` file.

There is a shell script `plot-ast.sh` to plot ast graph in PNG format, and an automated script `run-all.sh` which plots all `.cmm` files under directory `input-examples/inputs/`, a `.log` file will be generated for all errors, and plots will be at the same directory of inputs.