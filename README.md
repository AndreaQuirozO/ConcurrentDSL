# Concurrent DSL with Static Deadlock Freedom

A Domain-Specific Language (DSL) in OCaml for shared-memory concurrency with explicit locks. A type system, inspired by Kobayashi's work on deadlock-free process calculi [1, 2, 3, 4] and related extensions and specializations for lock-based languages [5], rejects programs that can deadlock before they run.

Developed as the course project for CS-462: Programming Language Foundations at EPFL.

---

## Authors

* **Tommaso Capone**
* **Laura Andrea Quiroz Ortega**

---

## Overview

Concurrent programming with shared memory and explicit locks is prone to deadlocks (circular waits, locks held across a `join`, and so on). This project implements:

* **Concurrency primitives**: thread creation (`spawn`), synchronization (`join`), and non-block-structured locks (`create_lock`, `lock`, `unlock`).
* **Formal small-step semantics**: a local step relation (→) for one thread and a non-deterministic global step relation (⇒) for the whole program, over a shared heap.
* **Static deadlock elimination**: a type system with lock/thread levels and lock obligations. Levels are **inferred**: the checker generates constraints `level(a) < level(b)` and rejects a program if they contain a cycle.
* **An interpreter that can explore every schedule**, used to check the type system against real behaviour.

The type system guarantees **deadlock freedom**. It does not guarantee data-race freedom: locks are plain mutexes and are not tied to specific variables.

---

## Language Syntax

Abstract syntax:

```text
a ::= x | n | a1 + a2 | a1 - a2 | a1 * a2

b ::= true | false
    | a1 = a2 | a1 != a2
    | a1 < a2 | a1 <= a2 | a1 > a2 | a1 >= a2
    | not b | b1 and b2 | b1 or b2

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

Concrete syntax (what the parser accepts):

* The bodies of `if`, `while` and `spawn` must be written in braces, e.g. `while i < n do { i := i + 1 }`. An empty body `{ }` means `skip`.
* `if` always needs an `else`.
* Statements are separated by `;` (also after a closing `}`). A trailing `;` is allowed.
* Variables start with a lowercase letter, followed by lowercase letters, digits or `_`. Negative literals are written `-5`.
* Comments are `/* ... */` and do not nest.
* Reserved words: `skip if then else while do spawn join lock unlock create_lock true false not and or`.

Example:

```text
a := create_lock();
b := create_lock();
x := 0;
t := spawn {
  lock a; lock b;
  x := x + 1;
  unlock b; unlock a
};
lock a; lock b;
x := x + 10;
unlock b; unlock a;
join t
```

---

## Semantics

* A thread configuration is `γ = ⟨σI, σL, σT, λ, c⟩`: stores for integers (variable → heap address), locks (variable → lock id) and threads (variable → thread id), the set `λ` of locks the thread holds, and the command `c`.
* A global configuration is `⟨Π, h⟩`: the thread map `Π` and the shared heap `h`. Thread `0` is the main thread.
* `spawn` copies the parent's `σI` and `σL` into the child, so parent and child share the heap cells that existed at the spawn point. Variables allocated later stay private by name.
* Assignment is the only operation that writes to the heap (`AssignOld` updates a cell, `AssignNew` allocates one).
* `lock` blocks while any thread holds the lock (locks are non-reentrant). `join` blocks until the child has terminated.
* A configuration is a **deadlock** when some thread is blocked and no thread can move.

The rules are written out in the report (`doc/documentation.pdf`).

---

## Type System

The judgment is `Γ, H, base ⊢ c ⇒ Γ', H'`, where `Γ` maps variables to `int`, `lock(lev)` or `th(lev)`, `H` is the set of locks currently held, and `base` is the minimum level the current thread may acquire.

* `lock x` and `join t` require that every held lock has a strictly lower level, and that the level is at least `base`.
* A spawned thread is checked with no held locks and `base = lev + 1`, so it can only take locks consistent with a later `join`. It must release every lock it takes.
* `if` branches must end with the same set of held locks. A `while` body must leave `H` unchanged and must not join threads.
* A program is well typed if `∅, ∅, 0 ⊢ c ⇒ Γ, ∅`.

**Limitations:** the system is conservative (some deadlock-free programs are rejected, for example a lock that is never released). It tracks lock obligations, not thread obligations, so an unjoined thread is allowed. It does not prevent data races.

The type sytem is written out in the report (`doc/documentation.pdf`).

---

## Project Structure

```text
.
├── bin/
│   └── main.ml              # CLI: run, type check, explore schedules
├── src/
│   ├── ast.ml               # Abstract syntax tree
│   ├── lexer.mll            # ocamllex tokenizer
│   ├── parser.mly           # ocamlyacc grammar
│   ├── pprint.ml            # Pretty printer (parsable output)
│   ├── interpreter.ml       # Small-step semantics: step (→), successors (⇒), run, explore
│   ├── typechecker.ml       # Type rules + level inference (cycle detection)
│   └── dune
├── tests/
│   ├── deadlock/
│   │   ├── positive/        # Programs that must be accepted and never deadlock
│   │   └── negative/        # Programs that must be rejected and really can deadlock
│   ├── functional/          # Programs (*.dsl) with expected output (*.expected), run by `make functional`
│   └── validation/          # OCaml unit tests, one file per module
│       ├── helpers.ml       # Shared test harness
│       ├── test_lexer.ml
│       ├── test_parser.ml
│       ├── test_pprint.ml
│       ├── test_interpreter.ml
│       ├── test_smallstep.ml    # one test per semantic rule
│       ├── test_typechecker.ml
│       └── dune
├── doc/
│   └── design.pdf           # CS-462 design document and reports
├── dune-project
├── Makefile
└── README.md
```

### Core Components

1. **AST**: algebraic data types for expressions, commands, threads and locks.
2. **Lexer & Parser**: `ocamllex` and `ocamlyacc` turn source text into an AST.
3. **Interpreter**: `step` implements the local rules, `successors` the global rule, `run` follows one schedule (random or first), and `explore` follows **every** schedule and returns each reachable `(status, final heap)`. Status is `Terminated`, `Deadlock` or `OutOfFuel`. Faults (unbound variable, `unlock` of a lock not held, `join` of a missing thread) raise `Runtime_error`.
4. **Type checker**: runs the typing rules, collects level constraints, and rejects programs whose constraints contain a cycle. The error message names the conflicting locks. On success it can report one valid level assignment (`check_levels`).

---

## Getting Started

### Prerequisites

* [OCaml](https://ocaml.org/) 4.14 or later
* `dune` (3.0 or later)
* `make`

`ocamllex` and `ocamlyacc` come with the standard OCaml distribution.

### Build

```bash
git clone <repo-url>
cd <repo>
make            # builds ./dsl (a copy of _build/default/bin/main.exe)
```

---

## Running the Language

```bash
./dsl program.dsl                 # run one random schedule, print final variables and status
./dsl --check program.dsl         # type check only
./dsl --explore program.dsl       # type check, then try every schedule for a deadlock
./dsl --deadlocks program.dsl     # skip the type check, try every schedule for a deadlock
```

Exit codes:

| Code | Meaning |
|---|---|
| 0 | success |
| 1 | type error or runtime error |
| 2 | usage, lexical or syntax error |
| 3 | a deadlock is possible in some schedule |

`--explore` and `--deadlocks` are exponential in program size, so use them on small programs. `./dsl program.dsl` runs a single random interleaving and can miss a deadlock that only some schedules reach.

---

## Running Tests

```bash
dune build && dune test --force   # OCaml unit tests
make test                         # deadlock tests
make functional                   # functional tests
```

**Unit tests (`tests/validation/`)**

| File | What it checks |
|---|---|
| `test_lexer.ml` | tokens, keywords vs. identifiers, longest match, comments, error positions |
| `test_parser.ml` | exact AST shapes, precedence, associativity, rejection of malformed programs |
| `test_pprint.ml` | printer output and `parse (print ast) = ast` |
| `test_smallstep.ml` | each semantic rule on a hand-built configuration: resulting state, blocked cases, faults |
| `test_interpreter.ml` | whole programs under every schedule: final heaps, deadlocks, faults |
| `test_typechecker.ml` | accepted/rejected programs, and soundness against `explore` |

The type checker tests check each program twice: the checker's verdict, and the interpreter's `explore` result, so a label never depends on the checker alone:

* **positive**: accepted, and no schedule deadlocks.
* **negative**: rejected, and some schedule really deadlocks.
* **conservative**: rejected, although no schedule deadlocks (shows the system is not complete).

**File-based tests (`make test`)**

* `tests/deadlock/positive/*.dsl` must be accepted and have no deadlock in any schedule.
* `tests/deadlock/negative/*.dsl` must be rejected by the type checker **and** deadlock in some schedule.

To add a case, drop a `.dsl` file in the right folder. A syntax error in a test file is reported as `FAILED`, not as a rejection.

**Functional tests (`make functional`)**

* Each `tests/functional/name.dsl` is run with `./dsl` and its output must match `tests/functional/name.expected` exactly (final variables sorted by name, then the status, e.g. `x = 6` / `terminated`).
* Programs must be well typed and give the same result in every schedule, since `./dsl` runs a single random one.

To add a case, write `name.dsl`, work out its expected output by hand, and save it as `name.expected`.

---

## Roadmap & Milestones

* [x] **Proposal**: initial proposal and design sketch.
* [x] **Check-In #1 (Design)**: syntax, operational semantics (→, ⇒), type system draft, repo set-up.
* [x] **Check-In #2 (Prototype)**: AST, `ocamllex`/`ocamlyacc` parser, small-step interpreter, type system formalization and implementation, deadlock test suite.
* [ ] **Check-In #3 (Release Candidate)**: soundness proof, final test suite, report.
* [ ] **Final Submission**: final report.

---

## References

[1] Naoki Kobayashi. “A new type system for deadlock-free processes”. In: International Conference on Concurrency Theory. Springer. 2006, pp. 233–247.

[2] Naoki Kobayashi. “Type-based information flow analysis for the π-calculus”. In: Acta Informatica 42.4 (2005), pp. 291–347.

[3] Naoki Kobayashi, Shin Saito, and Eijiro Sumii. “An implicitly-typed deadlock-free process calculus”. In: International Conference on Concurrency Theory. Springer. 2000, pp. 489–504.

[4] Eijiro Sumii and Naoki Kobayashi. “A generalized deadlock-free process calculus”. In: Electronic Notes in Theoretical Computer Science 16.3 (1998), pp. 225–247.

[5] Suenaga, K. (2008). Type-Based Deadlock-Freedom Verification for Non-Block-Structured Lock Primitives and Mutable References. In: Ramalingam, G. (eds) Programming Languages and Systems. APLAS 2008. Lecture Notes in Computer Science, vol 5356. Springer, Berlin, Heidelberg. https://doi.org/10.1007/978-3-540-89330-1_12