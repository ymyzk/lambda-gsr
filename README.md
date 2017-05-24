# lambda-gsr

[![Build Status](https://travis-ci.org/ymyzk/lambda-gsr.svg?branch=master)](https://travis-ci.org/ymyzk/lambda-gsr)

An interpreter of the implicitly and gradually typed language with shift and reset.

<!-- **Try it online!! https://gsrinfer.ymyzk.com** -->

## Build instructions
```shell
omake
./main/main
```

### Unit tests
```shell
omake test
```

## Syntax
If type annotations are omitted, they are recovered by the type reconstruction algorithm.

- Types (U):
  - Base types: `bool`, `int`, `unit`
  - Dynamic type: `?`
  - Function type: `U/U -> U/U`
- Variables: Lowercase IDs (e.g., `x`, `this_is_var`, `f0`)
- Constants: integers, `true`, `false`, `()`
- Binary operators: `+`, `-`, `*`, `/`, `=`, `>`, `<`
- Abstraction:
  - `fun^U (x: U) -> e`
  - `fun^U x -> e`
  - `fun (x: U) -> e`
  - `fun x -> e`
- Application: `e1 e2`
- Shift: `shift (k: U) -> e`, `shift k -> e`
- Reset: `reset^U e`, `reset e`
- If: `if e1 then e2 else e3`
- Sequence: `e1; e2`
- Top-level input:
  - `e;;`
  - `#debug true;;`, `#debug false;;`

## Requirements
- OCaml 4.02+
- OMake
- Menhir
- OUnit2
- js_of_ocaml

## References
- Yusuke Miyazaki and Atsushi Igarashi. A type reconstruction algorithm for gradually typed delimited continuations. In 第19回プログラミングおよびプログラミング言語ワークショップ (PPL2017) 論文集, 2017.
- [Ronald Garcia and Matteo Cimini. Principal Type Schemes for Gradual Programs. In Proc. of ACM POPL, 2015.](http://www.cs.ubc.ca/~rxg/ptsgp.pdf)
