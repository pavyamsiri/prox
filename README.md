# Prox

`prox` is a Rust implementation of the Lox programming language from Bob Nystrom's book "Crafting Interpreters".

## Usage

`prox` provides several subcommands.

```bash
# Run a Lox file with the bytecode VM
prox run program.lox

# Run with debug mode to see bytecode execution
prox run --debug program.lox

# Run using the tree-walk interpreter
prox walk program.lox

# Tokenize a file and see the token stream
prox tokenize program.lox

# Parse a file and see the concrete syntax tree
prox parse program.lox

# Compile to bytecode and show disassembly
prox disassemble program.lox
```

## Tests

The tests are based off the test suite found in the [CraftingInterpreters](https://github.com/munificent/craftinginterpreters)
repo.

