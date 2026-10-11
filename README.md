# Concurrent DSL with Static Deadlock Freedom

A Domain-Specific Language (DSL) in OCaml that statically guarantees deadlock freedom in shared-memory concurrent environments using explicit locking primitives, thread spawning/joining, and Kobayashi-style [1, 2, 3] type inference.

Developed as the course project for CS-462: Programming Language Foundations at EPFL.

---

## Authors

* **Tommaso Capone**
* **Laura Andrea Quiroz Ortega**

---

## Overview

Concurrent programming with shared memory and explicit locks is notoriously prone to deadlocks (e.g., circular wait conditions). This project implements a custom imperative DSL featuring:

* **Concurrency Primitives**: Thread creation (`spawn`), synchronization (`join`), and non-block-structured lock primitives (`create_lock`, `lock`, `unlock`).
* **Formal Small-Step Semantics**: Separate local thread evaluation ($\rightarrow$) and non-deterministic global concurrent interleaving ($\Rightarrow$).
* **Static Deadlock Elimination**: A type system grounded in Kobayashi's [1, 2, 3] process calculus framework that tracks lock levels and usage capabilities/obligations to eliminate deadlocks at compile time.

---

## Language Syntax

```text
a ::= x | n | a1 + a2 | a1 * a2
b ::= true | false | a1 < a2
c ::= skip
    | x := a
    | c1; c2
    | if b then c1 else c2
    | while b do c
    | x := spawn c
    | join x
    | x := create_lock()
    | lock x
    | unlock x

```

---

## Project Structure

```text
.
├── src/
│   ├── ast.ml          # Abstract Syntax Tree definitions
│   ├── lexer.mll       # ocamllex source for tokenization
│   ├── parser.mly      # ocamlyacc grammar for parsing
│   ├── interpreter.ml  # Small-step local (→) and global (⇒) operational semantics
│   ├── typechecker.ml  # Static analyzer enforcing lock-ordering constraints
│   ├── pprint.ml       # Pretty-printing functions for AST nodes
│   └── main.ml         # Entry point / CLI
├── test/
│   ├── positive/       # Valid concurrent programs (should pass typechecker)
│   └── negative/       # Deadlocking programs (should be rejected statically)
├── doc/
│   └── design.pdf      # CS-462 formal design document
├── Makefile
└── README.md

```

### Core Components

1. **AST Definition**: Algebraic data types for expressions, commands, threads, and lock types.
2. **Lexer & Parser**: Built using `ocamllex` and `ocamlyacc` to transform raw text into structured ASTs.
3. **Interpreter**: Evaluates local state $\gamma = \langle \sigma_I, \sigma_L, \sigma_T, \lambda, c \rangle$ and global state $\langle \Pi, h \rangle$ following the formal small-step reduction rules.
4. **Type Checker**: Extracts lock-level inequality constraints and verifies usage reliability to accept or reject programs before execution.

<!-- ---

## Getting Started

### Prerequisites

* [OCaml](https://ocaml.org/) (v4.14 or later recommended)
* `dune` build system
* `ocamllex` and `ocamlyacc` (included with standard OCaml tooling)
* `ounit2` (optional, for running unit tests)

### Installation

Clone the repository and build the executable:

```bash
git clone [https://github.com/your-username/your-repo-name.git](https://github.com/your-username/your-repo-name.git)
cd your-repo-name
dune build

```

---

## Running the Language

To parse, type-check, and interpret a program file:

```bash
dune exec bin/main.exe -- path/to/program.dsl

```

### Options

* `--ast` : Print the parsed AST.
* `--typecheck-only` : Run static analysis without executing the interpreter.
* `--trace` : Print small-step state transitions ($\rightarrow$ and $\Rightarrow$) during execution.

---

## Running Tests

Execute the test suite using Dune:

```bash
dune runtest

```

The test suite includes:

* **Positive Tests**: Correct concurrent algorithms (e.g., dining philosophers with properly ordered locks).
* **Negative Tests**: Intentional circular waits (e.g., Thread 1 locks $A \to B$ while Thread 2 locks $B \to A$), which must be rejected by the type checker prior to evaluation.

 -->

 ---

## Roadmap & Milestones

* [x] **Proposal**: Initial proposal and design sketch.
* [ ] **Check-In #1 (Design)**: Syntax, operational semantics ($\rightarrow, \Rightarrow$), type system draft, and repo set-up.
* [ ] **Check-In #2 (Prototype)**: AST, `ocamllex`/`ocamlyacc` parser, and small-step interpreter.
* [ ] **Check-In #3 (Release Candidate)**: Integrated type checker, constraint solver, soundness proof draft and test suite.
* [ ] **Final Submission**: Final report.

---

## References

[1] Naoki Kobayashi. “A new type system for deadlock-free processes”. In: International Conference on Concurrency Theory. Springer. 2006, pp. 233–247.

[2] Naoki Kobayashi. “Type-based information flow analysis for the π-calculus”. In: Acta Informatica 42.4 (2005), pp. 291–347.

[3] Naoki Kobayashi, Shin Saito, and Eijiro Sumii. “An implicitly-typed deadlock-free process calculus”. In: International Conference on Concurrency Theory. Springer. 2000, pp. 489–504.
