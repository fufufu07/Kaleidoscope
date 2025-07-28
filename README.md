# Kaleidoscope Compiler

This is a refactored version of the Kaleidoscope compiler from the LLVM tutorial. The code has been organized into multiple files for better maintainability.

## File Structure

### Header Files
- **Token.h** - Token enum definitions for the lexer
- **AST.h** - Abstract Syntax Tree node class definitions
- **Lexer.h** - Lexer function declarations and global state
- **Parser.h** - Parser function declarations

### Source Files  
- **Lexer.cpp** - Lexical analysis implementation
- **Parser.cpp** - Parser implementation with AST node creation
- **main.cpp** - Main application logic and handler functions

## Build Instructions

Requirements:
- CMake 3.15+
- C++23 compatible compiler
- Visual Studio 2022 (for Windows build)

Build steps:
```bash
cd Kaleidoscope
cmake -G "Visual Studio 17 2022" .
cmake --build .
```

## Usage

Run the executable and enter expressions:
```
ready> 2+3*4
Parsed a top-level expr.
ready> def foo(x y) x+y*2
Parsed a function definition.
ready> 
```

## Architecture

The compiler is structured into distinct phases:

1. **Lexical Analysis** (Lexer.cpp) - Breaks input into tokens
2. **Parsing** (Parser.cpp) - Builds Abstract Syntax Tree from tokens  
3. **Main Loop** (main.cpp) - Handles different input types (definitions, expressions, etc.)

This modular design makes it easy to extend with additional features like:
- Code generation to LLVM IR
- Optimization passes
- Additional language constructs

## Next Steps

This refactored code is ready for adding LLVM IR generation. The clean separation between lexing, parsing, and AST representation makes it straightforward to add a CodeGen phase that walks the AST and emits LLVM IR.
