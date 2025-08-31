//
// Created by Mt342 on 2025/8/22.
//

#include <algorithm>

#include "Lexer.h"
#include "Parser.h"
#include "Debug.h"  // 包含 CurLoc 的定义
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

  // 使用全局 CurLoc
  return std::make_unique<IfExprAST>(CurLoc, std::move(Cond), std::move(Then),
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

  return std::make_unique<ForExprAST>(CurLoc, IdName, std::move(Start),
                                      std::move(End), std::move(Step),
                                      std::move(Body));
}

/// ParseVarExpr - This function parses var/in expressions.
/// var identifier ('=' expression)? (',' identifier ('=' expression)?)* 'in' expression
std::unique_ptr<ExprAST> ParseVarExpr() {
  get_next_token(); // eat 'var'

  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;

  // At least one variable name is required.
  if (cur_tok != Token::k_tok_identifier)
    return LogError("expected identifier after var");

  while (true) {
    std::string Name = identifier_str;
    get_next_token(); // eat identifier

    // Read the optional initializer.
    std::unique_ptr<ExprAST> Init;
    if (cur_tok == static_cast<Token>('=')) {
      get_next_token(); // eat '='

      Init = ParseExpression();
      if (!Init) return nullptr;
    }

    VarNames.emplace_back(Name, std::move(Init));

    // End of this var list?
    if (cur_tok != static_cast<Token>(','))
      break;
    get_next_token(); // eat ','

    if (cur_tok != Token::k_tok_identifier)
      return LogError("expected identifier after ',' in var");
  }

  // At this point, we have to have 'in'.
  if (cur_tok != Token::k_tok_in)
    return LogError("expected 'in' keyword after 'var'");
  get_next_token(); // eat 'in'

  auto Body = ParseExpression();
  if (!Body)
    return nullptr;

  return std::make_unique<VarExprAST>(CurLoc, std::move(VarNames), std::move(Body));
}
