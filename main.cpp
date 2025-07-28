#include <print>

#include "AST.h"
#include "Lexer.h"
#include "Parser.h"

// Forward declarations for handlers
static void HandleDefinition();
static void HandleExtern();
static void HandleTopLevelExpression();

/// HandleDefinition - Handle function definitions
static void HandleDefinition() {
  if (auto fn_ast = ParseDefinition()) {
    std::println(stderr, "Parsed a function definition.");
  } else {
    // Skip token for error recovery.
    GetNextToken();
  }
}

/// HandleExtern - Handle external declarations
static void HandleExtern() {
  if (auto proto_ast = ParseExtern()) {
    std::println(stderr, "Parsed an extern.");
  } else {
    // Skip token for error recovery.
    GetNextToken();
  }
}

/// HandleTopLevelExpression - Handle top-level expressions
static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto fn_ast = WrapAsTopLevel(ParseExpression())) {
    std::println(stderr, "Parsed a top-level expr.");
  } else {
    // Skip token for error recovery.
    GetNextToken();
  }
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
  while (true) {
    std::print(stderr, "ready> ");
    switch (GetCurrentToken()) {
      case Token::kTokEof:
        return;
      case static_cast<Token>(';'):  // ignore top-level semicolons.
        GetNextToken();
        break;
      case Token::kTokDef:
        HandleDefinition();
        break;
      case Token::kTokExtern:
        HandleExtern();
        break;
      default:
        HandleTopLevelExpression();
        break;
    }
  }
}

int main() {
  // Initialize binary operator precedence.
  InitializeBinopPrecedence();

  // Prime the first token.
  std::print(stderr, "ready> ");
  GetNextToken();

  // Now run the main "interpreter loop".
  MainLoop();

  return 0;
}
