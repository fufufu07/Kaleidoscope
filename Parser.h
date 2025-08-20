#pragma once

#include <expected>
#include <memory>
#include <string_view>

#include "AST.h"

/// Expression parsing functions
std::unique_ptr<ExprAST> ParseExpression();
std::unique_ptr<ExprAST> ParseNumberExpr();
std::unique_ptr<ExprAST> ParseParenExpr();
std::unique_ptr<ExprAST> ParseIdentifierExpr();
std::unique_ptr<ExprAST> ParsePrimary();
std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS);

/// Prototype and function parsing
std::unique_ptr<PrototypeAst> ParsePrototype();
std::expected<std::unique_ptr<FunctionAST>, std::string_view> ParseDefinition();
std::expected<std::unique_ptr<PrototypeAst>, std::string_view> ParseExtern();

/// Utility functions
template <typename ExprType>
  requires std::derived_from<ExprType, ExprAST>
std::unique_ptr<FunctionAST> WrapAsTopLevel(std::unique_ptr<ExprType> expr);

/// Error logging
std::unique_ptr<ExprAST> LogError(std::string_view str);
std::unique_ptr<PrototypeAst> LogErrorP(std::string_view str);

/// Get token precedence
int GetTokPrecedence();

/// Initialize binary operator precedence
void InitializeBinopPrecedence();
