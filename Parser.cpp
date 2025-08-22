#include "Parser.h"

#include <map>
#include <print>

#include "Lexer.h"

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<Token, int> binop_precedence{};
std::unique_ptr<ExprAST> ParseIfExpr();

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(std::string_view str) {
  std::println(stderr, "Error: {}", str);
  return nullptr;
}

std::unique_ptr<PrototypeAst> LogErrorP(std::string_view str) {
  std::println(stderr, "Error: {}", str);
  return nullptr;
}

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
int GetTokPrecedence() {
  Token cur_tok = get_current_token();
  if (!isascii(static_cast<int>(cur_tok))) return -1;

  // Make sure it's a declared binop.
  const int tok_precedence = binop_precedence[cur_tok];
  if (tok_precedence <= 0) {
    return -1;
  }
  return tok_precedence;
}

/// numberexpr ::= number
std::unique_ptr<ExprAST> ParseNumberExpr() {
  auto result = std::make_unique<NumberExprAST>(num_val);
  get_next_token();  // consume the number
  return result;
}

/// parenexpr ::= '(' expression ')'
std::unique_ptr<ExprAST> ParseParenExpr() {
  get_next_token();  // eat (.
  auto expr = ParseExpression();
  if (!expr) {
    return nullptr;
  }

  if (get_current_token() != static_cast<Token>(')')) {
    return LogError("expected ')'");
  }
  get_next_token();  // eat ).
  return expr;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string id_name{identifier_str};  // Remember the identifier name

  get_next_token();  // eat identifier.

  if (get_current_token() != static_cast<Token>('(')) {  // Simple variable ref.
    return std::make_unique<VariableExprAST>(std::move(id_name));
  }

  // Call.
  get_next_token();                              // eat (
  std::vector<std::unique_ptr<ExprAST>> args;  // Argument expressions.

  if (get_current_token() !=
      static_cast<Token>(')')) {  // If the next token is not ')', we have to
    while (true) {                // Parse the arguments.
      if (auto arg = ParseExpression()) {
        args.push_back(std::move(arg));
      } else {
        return nullptr;
      }

      if (get_current_token() == static_cast<Token>(')')) {
        break;
      }

      if (get_current_token() != static_cast<Token>(',')) {
        return LogError("Expected ')' or ',' in argument list");
      }
      get_next_token();  // eat the ','
    }
  }

  // Eat the ')'.
  get_next_token();

  return std::make_unique<CallExprAST>(std::move(id_name), std::move(args));
}

/// ParsePrimary - This function handles primary expressions, which can be
/// identifiers, numbers, or parenthesized expressions.
/// @return A unique pointer to the parsed expression AST, or nullptr on error.
std::unique_ptr<ExprAST> ParsePrimary() {
  switch (get_current_token()) {
    case Token::kTokIdentifier:
      return ParseIdentifierExpr();
    case Token::kTokNumber:
      return ParseNumberExpr();
    case static_cast<Token>('('):
      return ParseParenExpr();
    case Token::kTokIf:
      return ParseIfExpr();
    default:
      return LogError("unknown token when expecting an expression");
  }
}

/// binoprhs
///   ::= ('+' primary)*
std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                       std::unique_ptr<ExprAST> LHS) {
  // If this is a binop, find its precedence.
  while (true) {
    const int tok_prec =
        GetTokPrecedence();  // Get the precedence of the current token.

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (tok_prec < ExprPrec) {
      return LHS;
    }
    // Okay, we know this is a binop.
    Token bin_op = get_current_token();
    get_next_token();  // eat binop

    // Parse the primary expression after the binary operator.
    auto rhs{ParsePrimary()};
    if (!rhs) {
      return nullptr;
    }
    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    if (const int next_prec{GetTokPrecedence()}; tok_prec < next_prec) {
      rhs = ParseBinOpRHS(tok_prec + 1, std::move(rhs));
      if (!rhs) {
        return nullptr;
      }
    }

    // Merge LHS/RHS.
    LHS =
        std::make_unique<BinaryExprAST>(bin_op, std::move(LHS), std::move(rhs));
  }  // loop around to the top of the while loop.
}

/// expression
///   ::= primary binoprhs
///
std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParsePrimary();
  if (!LHS) return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}

/// prototype
///   ::= id '(' id* ')'
std::unique_ptr<PrototypeAst> ParsePrototype() {
  // 使用结构化绑定和期望类型
  if (get_current_token() != Token::kTokIdentifier) [[unlikely]] {
    return LogErrorP("Expected function name in prototype");
  }

  auto fn_name = std::string{identifier_str};
  get_next_token();

  if (get_current_token() != Token::kTokLParen) [[unlikely]] {
    return LogErrorP("Expected '(' in prototype");
  }

  // 可以考虑预留空间
  std::vector<std::string> arg_names;
  arg_names.reserve(8);  // 大多数函数参数不会超过8个

  while (get_next_token() == Token::kTokIdentifier) {
    arg_names.emplace_back(identifier_str);
  }

  if (get_current_token() != Token::kTokRParen) [[unlikely]] {
    return LogErrorP("Expected ')' in prototype");
  }

  get_next_token();
  return std::make_unique<PrototypeAst>(std::move(fn_name),
                                        std::move(arg_names));
}

/// definition ::= 'def' prototype expression
std::expected<std::unique_ptr<FunctionAST>, std::string_view>
ParseDefinition() {
  get_next_token();

  auto proto = ParsePrototype();
  if (!proto) {
    return std::unexpected("Failed to parse function prototype");
  }

  auto body = ParseExpression();
  if (!body) {
    return std::unexpected("Failed to parse function body");
  }

  return std::make_unique<FunctionAST>(std::move(proto), std::move(body));
}

/// external ::= 'extern' prototype
std::expected<std::unique_ptr<PrototypeAst>, std::string_view>
ParseExtern() {
  get_next_token();  // eat extern.

  auto proto = ParsePrototype();
  if (!proto) [[unlikely]] {
    return std::unexpected("Failed to parse extern prototype");
  }
  return proto;
}

/// toplevelexpr ::= expression
template <typename ExprType>
  requires std::derived_from<ExprType, ExprAST>
std::unique_ptr<FunctionAST> WrapAsTopLevel(
    std::unique_ptr<ExprType> expr) {
  auto proto =
      std::make_unique<PrototypeAst>("__anon_expr", std::vector<std::string>{});
  return std::make_unique<FunctionAST>(std::move(proto), std::move(expr));
}

// Explicit template instantiation for ExprAST
template std::unique_ptr<FunctionAST> WrapAsTopLevel<ExprAST>(std::unique_ptr<ExprAST> expr);

// Initialize binary operator precedence
void InitializeBinopPrecedence() {
  // Install standard binary operators.
  // 1 is lowest precedence.
  binop_precedence[static_cast<Token>('<')] = 10;
  binop_precedence[static_cast<Token>('+')] = 20;
  binop_precedence[static_cast<Token>('-')] = 20;
  binop_precedence[static_cast<Token>('*')] = 40;  // highest.
}
