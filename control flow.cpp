//
// Created by Mt342 on 2025/8/22.
//

#include <algorithm>

#include "Lexer.h"
#include "Parser.h"
class IfExprAST;
extern Token cur_tok;

/// ParsePrimary - This function parses primary expressions like numbers, identifiers, or parenthesized expressions.
std::unique_ptr<ExprAST> ParseIfExpr() {
  get_next_token();  // eat the if.

  // condition.
  auto Cond = ParseExpression();
  if (!Cond) {
    return nullptr;
  }
  // Check for 'then' token.
  if (cur_tok != Token::k_tok_then) {
    return LogError("expected then");
  }
  get_next_token();  // eat the then

  auto Then = ParseExpression();
  if (!Then) {
    return nullptr;
  }

  if (cur_tok != Token::k_tok_else) {
    return LogError("expected else");
  }

  get_next_token();

  auto Else = ParseExpression();
  if (!Else) {
    return nullptr;
  }

  return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then),
                                     std::move(Else));
}

/// ParseForExpr - This function parses for/in expressions.
/// @return A unique pointer to the ForExprAST representing the parsed for expression, or nullptr on error.
std::unique_ptr<ExprAST> ParseForExpr() {
  get_next_token();  // eat the for.

  if (cur_tok != Token::k_tok_identifier) {
    return LogError("expected identifier after for");
  }

  std::string IdName = identifier_str;
  get_next_token();  // eat identifier.

  if (cur_tok != static_cast<Token>('=')) {
    return LogError("expected '=' after for");
  }
  get_next_token();  // eat '='.

  // The start expression.
  auto Start = ParseExpression();
  if (!Start) {
    return nullptr;
  }
  if (cur_tok != static_cast<Token>(',')) {
    return LogError("expected ',' after for start value");
  }
  get_next_token();

  auto End = ParseExpression();
  if (!End) {
    return nullptr;
  }

  // The step value is optional.
  std::unique_ptr<ExprAST> Step;
  if (cur_tok == static_cast<Token>(',')) {
    get_next_token();
    Step = ParseExpression();
    if (!Step) {
      return nullptr;
    }
  }

  if (cur_tok != Token::k_tok_in) {
    return LogError("expected 'in' after for");
  }
  get_next_token();  // eat 'in'.

  auto Body = ParseExpression();
  if (!Body) {
    return nullptr;
  }

  return std::make_unique<ForExprAST>(IdName, std::move(Start),
                                      std::move(End), std::move(Step),
                                      std::move(Body));
}