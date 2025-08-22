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
  if (cur_tok != Token::kTokThen) {
    return LogError("expected then");
  }
  get_next_token();  // eat the then

  auto Then = ParseExpression();
  if (!Then) {
    return nullptr;
  }

  if (cur_tok != Token::kTokElse) {
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