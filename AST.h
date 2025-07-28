#pragma once

#include <memory>
#include <string>
#include <vector>

#include "Token.h"

/// ExprAST - Base class for all expression nodes.
class ExprAST {
 public:
  virtual ~ExprAST() = default;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  double val_;

 public:
  explicit NumberExprAST(double val) noexcept : val_(val) {}

  [[nodiscard]] double GetValue() const noexcept { return val_; }
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string name_;

 public:
  explicit VariableExprAST(std::string name) : name_(std::move(name)) {}

  [[nodiscard]] const std::string& GetName() const noexcept { return name_; }
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  Token op_;
  std::unique_ptr<ExprAST> lhs_, rhs_;

 public:
  BinaryExprAST(Token op, std::unique_ptr<ExprAST> lhs,
                std::unique_ptr<ExprAST> rhs) noexcept
      : op_(op), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {}

  [[nodiscard]] Token GetOp() const noexcept { return op_; }
  [[nodiscard]] const ExprAST* GetLHS() const noexcept { return lhs_.get(); }
  [[nodiscard]] const ExprAST* GetRHS() const noexcept { return rhs_.get(); }
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string callee_;
  std::vector<std::unique_ptr<ExprAST>> args_;

 public:
  CallExprAST(std::string callee, std::vector<std::unique_ptr<ExprAST>> args)
      : callee_(std::move(callee)), args_(std::move(args)) {}

  [[nodiscard]] const std::string& GetCallee() const noexcept {
    return callee_;
  }
  [[nodiscard]] const std::vector<std::unique_ptr<ExprAST>>& GetArgs()
      const noexcept {
    return args_;
  }
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAST {
  std::string name_;
  std::vector<std::string> args_;

 public:
  PrototypeAST(std::string name, std::vector<std::string> args)
      : name_(std::move(name)), args_(std::move(args)) {}

  [[nodiscard]] const std::string& GetName() const noexcept { return name_; }
  [[nodiscard]] const std::vector<std::string>& GetArgs() const noexcept {
    return args_;
  }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> proto_;  // 函数声明
  std::unique_ptr<ExprAST> body_;        // 函数体

 public:
  FunctionAST(std::unique_ptr<PrototypeAST> proto,
              std::unique_ptr<ExprAST> body) noexcept
      : proto_(std::move(proto)), body_(std::move(body)) {}

  [[nodiscard]] const PrototypeAST* GetProto() const noexcept {
    return proto_.get();
  }
  [[nodiscard]] const ExprAST* GetBody() const noexcept { return body_.get(); }
};
