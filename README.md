# FraJer Language Interpreter

An educational, imperative, C‑style language for performing computations. It features a minimal static type system plus pragmatic constructs for experimentation, all implemented in Haskell using BNFC + Alex + Happy.

---

## Project Authors

Franciszek Wrzosek, Jerzy Szempliński

---

## Language Overview (Detailed)

FraJer is intentionally small yet expressive:

* Syntax: Familiar, C / C++ inspired.
* Primitive Types: `Int`, `Bool`.
* Containers: Arrays and dictionaries (statically type‑checked usage).
* Functions: 
	* Can be declared inside blocks (nested lexical scopes).
	* Anonymous functions (lambdas) may be supplied inline as arguments.
* Expressions with side effects: Certain expressions mutate state directly to make numeric workflows concise.
* `swap a b;`: Swaps the *locations* (not just the values) of two variables within the current environment snapshot. Local to scope: swapping inside a function / loop does not affect bindings outside.
* Debug flags: Per‑variable opt‑in tracing for writes and/or reads to aid understanding and grading; emitted in a controlled, separated fashion (not mixed with semantic errors).
* Extended loop control:
	* `break n;` exits `n` nested loops in one step.
	* `continue outer (n);` unwinds `n` loops, then performs a `continue` on the next outer loop.
* Error Discipline: All static (type / name) and runtime (e.g. division by zero, modulo by zero) errors are captured—no uncaught Haskell exceptions permitted. Normal output goes to `stdout`; diagnostics to `stderr`.
* Semantics: Designed to be compositional / denotational in spirit, making formal reasoning and extension straightforward.

---

## Key Language Features

| Category | Feature | Notes |
|----------|---------|-------|
| Types | `Int`, `Bool` | Primitive static types |
| Data | Arrays, Dictionaries | Heterogeneity restricted by static typing |
| Functions | Nested declarations, anonymous (lambda) parameters | Lexically scoped; shadowing tested thoroughly |
| Side Effects | Expressions may mutate state | Designed for numeric convenience |
| Swap | `swap a b;` | Swaps *locations* locally; changes do not escape function/loop scope |
| Flow Control | `if`, `while`, `for`, `break`, `continue` | Plus: `break n` (exit n loops), `continue outer (n)` (pop n loops then continue) |
| Debug Flags | Trace variable writes & reads | Opt‑in instrumentation for understanding execution |
| Safety | Static type checking + runtime checks | No uncaught Haskell exceptions allowed |
| Semantics | Denotationally inspired, compositional | Facilitates reasoning & extension |

---

## Repository Layout

```
Makefile            Build entry point (BNFC + Alex + Happy + GHC)
src/FraJer.cf       Grammar (BNFC specification)
src/FraJer/*        Generated + support parser / lexer modules
src/TypeChecker.hs  Static analysis & type rules
src/Executor.hs     Runtime / evaluation layer
src/Interpreter.hs  Main wiring (argument handling, IO)
good/*.fj           Valid sample programs (with .out / .err expectations)
bad/*.fj            Programs that should fail statically or at runtime
test.sh             Batch test runner (compares stdout/stderr)
review_tests.sh     (Auxiliary review script, if used)
```

Each test consists of **three files sharing a prefix**:

```
NNN_name.fj   Source program
NNN_name.out  Expected stdout (may be empty)
NNN_name.err  Expected stderr (may be empty)
```

`good/` contains examples that should type‑check and (if they run) terminate successfully. `bad/` holds programs demonstrating type errors or runtime faults (e.g. division by zero) – the interpreter must report these gracefully to `stderr`.

---

## Toolchain Versions (Known Good)

| Tool | Version | Notes |
|------|---------|-------|
| GHC | 9.0.2 | Matches Stack resolver (e.g. lts-19.x) |
| Cabal | 3.4.1.0 | Shipped with the above GHC toolchain |
| Stack | 2.7.5 | Used to run BNFC / build via `stack exec` |
| BNFC | 2.9.5 | Grammar to Haskell modules |
| Alex | 3.2.7.1 | Lexer generator |
| Happy | 1.20.0 | Parser generator |

Newer versions often work, but these are validated.

---

## Prerequisites & Installation (High‑Level)

> IMPORTANT: You must have the listed tool versions (or very close) installed. Newer major versions sometimes introduce breaking changes and the build may fail. If something does not work, first downgrade / align versions before opening an issue.

Required tooling (see version table above): GHC, Cabal (bundled), Stack, BNFC, Alex, Happy.

Official installation / upgrade references (use these instead of ad‑hoc scripts):

* GHC / Cabal (multi‑version management): https://www.haskell.org/ghcup/install/
* Stack: https://docs.haskellstack.org/en/stable/install_and_upgrade/
* BNFC: https://bnfc.digitalgrammars.com/ (Hackage: https://hackage.haskell.org/package/bnfc)
* Alex: https://www.haskell.org/alex/
* Happy: https://www.haskell.org/happy/

After installing, confirm versions (example expected output forms):
```bash
ghc --version       # Should show 9.0.2 (or compatible)
stack --version     # Matches known working Stack release
bnfc --version      # 2.9.5
alex --version      # 3.2.7.1
happy --version     # 1.20.0
```

If a command is not found, adjust your PATH per the installer’s instructions (ghcup and Stack both print guidance). On Windows, prefer using Git Bash or WSL for running `make` and the test script.

Clone the repository once the environment is ready:
```bash
git clone <this-repo-url> FraJer
cd FraJer
```

---

## Build

Simple build (regenerates parser + compiles):
```bash
make
```
This will:
1. Run BNFC on `src/FraJer.cf` (generates `Abs.hs`, `Par.hs`, `Lex.hs`, etc.).
2. Run Alex & Happy on the produced `.x` / `.y` sources.
3. Compile everything with GHC into the executable `./interpreter`.

Clean generated + object files:
```bash
make clean
```

> If regeneration fails, ensure no stale partially generated modules remain (use `make clean`); then rebuild.

---

## Quick Start (Run a Sample)

After `make` succeeds:
```bash
./interpreter ./good/001_print_1.fj
```
Expected behavior: program executes; its normal output appears on `stdout`; any diagnostics or intentional error messages would go to `stderr` (none for this simple example).

Another example (type error, from `bad/`):
```bash
./interpreter ./bad/013_wrong_type_assign1.fj
```
This should produce no Haskell crash; instead a structured semantic / type error is printed to `stderr`.

---

## Running the Entire Test Suite

The script `test.sh` executes all `.fj` programs under `good/` and `bad/`, comparing actual `stdout` / `stderr` with the paired `.out` / `.err` files.

```bash
./test.sh
```

Typical responsibilities of `test.sh` (as required by the assignment brief):
1. For each `*.fj` file: run `./interpreter file.fj > TMP_OUT 2> TMP_ERR`.
2. `diff` the captured outputs against `file.out` and `file.err`.
3. Report per‑test success / failure succinctly.

You can focus on a single test manually:
```bash
./interpreter ./bad/029_expr_div_by_0.fj > /tmp/o 2> /tmp/e
cat /tmp/o
cat /tmp/e
```

---

## Error Handling Contract

Interpreter MUST NOT terminate with an unhandled Haskell exception. All issues are categorized:

| Kind | Examples | Destination |
|------|----------|-------------|
| Static (compile-time) | undeclared variable, type mismatch, shadowing misuse | `stderr` |
| Runtime | division/modulo by zero, invalid container access | `stderr` |
| Normal program output | `print`, evaluated expressions with side effects | `stdout` |

Empty `.out` / `.err` files are valid and mean “no output expected on that stream”.

---

## Semantics & Scoping Notes

* **Shadowing:** Inner scopes may redeclare identifiers. When the inner scope ends, the outer binding (with its preserved value) becomes visible again. Tests explicitly demonstrate this re‑emergence.
* **swap:** Operates on *locations* within the *current* environment snapshot. Because of lexical scoping, swapping inside a function or loop does not mutate bindings outside that scope (tests cover locality).
* **Extended loop control:**
	* `break n;` exits `n` nested loop levels immediately.
	* `continue outer (n);` unwinds `n` loops, then issues a `continue` on the next outer loop.
* **Debug flags:** When enabled for a variable, reads / writes are logged (implementation detail: instrumentation in executor; output destination may vary but should remain distinct from semantic errors).

---

## References

- [BNFC](https://bnfc.digitalgrammars.com/) – Grammar to parser/AST generator
- [Alex](https://www.haskell.org/alex/) – Lexer generator
- [Happy](https://www.haskell.org/happy/) – Parser generator
- [GHC](https://www.haskell.org/ghc/) – Haskell compiler

---

## Quick Commands (Assuming Correct Tool Versions Are Already Installed)

```bash
# Build (generates parser + compiles)
make

# Run a sample program
./interpreter ./good/001_print_1.fj

# Run the entire test suite
./test.sh
```

If anything fails unexpectedly, re‑check tool versions first (`ghc --version`, `bnfc --version`, etc.).

---
