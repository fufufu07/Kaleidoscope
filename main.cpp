#include <KaleidoscopeJIT.h>

#include <print>

#include "AST.h"
#include "Lexer.h"
#include "Parser.h"

// Forward declarations for handlers
void HandleDefinition();
void HandleExtern();
void HandleTopLevelExpression();

extern std::unique_ptr<llvm::orc::KaleidoscopeJIT> the_jit;
extern llvm::ExitOnError exit_on_err;


/// HandleDefinition - Handle function definitions
void HandleDefinition() {
  if (auto fn_ast = ParseDefinition()) {
    std::println(stderr, "Parsed a function definition.");
  } else {
    // Skip token for error recovery.
    get_next_token();
  }
}

/// HandleExtern - Handle external declarations
void HandleExtern() {
  if (auto proto_ast = ParseExtern()) {
    std::println(stderr, "Parsed an extern.");
  } else {
    // Skip token for error recovery.
    get_next_token();
  }
}
/// WrapAsTopLevel - Wrap an expression as a top-level function
std::unique_ptr<FunctionAST> parse_top_level_expr() {
  if (auto expr = ParseExpression()) {
    return WrapAsTopLevel(std::move(expr));
  }
  return nullptr;
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
  while (true) {
    std::print(stderr, "ready> ");
    switch (get_current_token()) {
      case Token::k_tok_eof:
        return;
      case static_cast<Token>(';'):  // ignore top-level semicolons.
        get_next_token();
        break;
      case Token::k_tok_def:
        HandleDefinition();
        break;
      case Token::k_tok_extern:
        HandleExtern();
        break;
      default:
        HandleTopLevelExpression();
        break;
    }
  }
}

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X) {
  fputc((char)X, stderr);
  return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}

int main() {
  // Initialize binary operator precedence.
  InitializeBinopPrecedence();

  // Prime the first token.
  std::print(stderr, "ready> ");
  get_next_token();

  // Initialize the LLVM context, module, and builder.
  the_jit = exit_on_err(llvm::orc::KaleidoscopeJIT::Create());
  // Now run the main "interpreter loop".
  MainLoop();

  return 0;
}
