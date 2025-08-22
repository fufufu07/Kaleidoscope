#pragma once

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "Token.h"
#include "llvm/IR/Value.h"

/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  virtual ~ExprAST() = default;

  virtual llvm::Value* codegen() = 0; /// Generate LLVM code for this expression
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  double val_;

public:
  explicit NumberExprAST(double val) noexcept : val_(val) {
  }

  [[nodiscard]] double GetValue() const noexcept { return val_; }

  llvm::Value* codegen() override;
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string name_;

public:
  explicit VariableExprAST(std::string name) : name_(std::move(name)) {
  }

  [[nodiscard]] const std::string& GetName() const noexcept { return name_; }

  llvm::Value* codegen() override;
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  Token op_;
  std::unique_ptr<ExprAST> lhs_, rhs_;

public:
  BinaryExprAST(Token op, std::unique_ptr<ExprAST> lhs,
                std::unique_ptr<ExprAST> rhs) noexcept
    : op_(op), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {
  }

  [[nodiscard]] Token GetOp() const noexcept { return op_; }
  [[nodiscard]] const ExprAST* GetLHS() const noexcept { return lhs_.get(); }
  [[nodiscard]] const ExprAST* GetRHS() const noexcept { return rhs_.get(); }

  llvm::Value* codegen() override;
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string callee_; // Function name being called
  std::vector<std::unique_ptr<ExprAST> > args_; // Arguments to the function call

public:
  CallExprAST(std::string callee, std::vector<std::unique_ptr<ExprAST> > args)
    : callee_(std::move(callee)), args_(std::move(args)) {
  }

  [[nodiscard]] const std::string& GetCallee() const noexcept {
    return callee_;
  }

  [[nodiscard]] const std::vector<std::unique_ptr<ExprAST> >& GetArgs()
  const noexcept {
    return args_;
  }

  llvm::Value* codegen() override;
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAst {
  std::string name_;
  std::vector<std::string> args_;

public:
  PrototypeAst(std::string name, std::vector<std::string> args)
    : name_(std::move(name)), args_(std::move(args)) {
  }

  [[nodiscard]] const std::string& GetName() const noexcept { return name_; }

  [[nodiscard]] const std::vector<std::string>& GetArgs() const noexcept {
    return args_;
  }

  llvm::Function* codegen() const;
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAst> proto_; // 函数声明
  std::unique_ptr<ExprAST> body_;       // 函数体

public:
  FunctionAST(std::unique_ptr<PrototypeAst> proto,
              std::unique_ptr<ExprAST> body) noexcept
    : proto_(std::move(proto)), body_(std::move(body)) {
  }

  [[nodiscard]] const PrototypeAst* GetProto() const noexcept {
    return proto_.get();
  }

  [[nodiscard]] const ExprAST* GetBody() const noexcept { return body_.get(); }

  llvm::Function* codegen();
};

/// IfExprAST - This class represents an if expression in the AST.
/// It contains a condition expression, a then expression, and an else expression.
/// The code generation for this expression will generate LLVM IR for the if-then-else construct
class IfExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Cond;
  std::unique_ptr<ExprAST> Then;
  std::unique_ptr<ExprAST> Else;

public:
  IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
            std::unique_ptr<ExprAST> Else)
    : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

  llvm::Value *codegen() override;
};

/// ForExprAST - Expression class for for/in.
class ForExprAST : public ExprAST {
  std::string VarName;
  std::unique_ptr<ExprAST> Start, End, Step, Body;

public:
  ForExprAST(std::string VarName, std::unique_ptr<ExprAST> Start,
             std::unique_ptr<ExprAST> End, std::unique_ptr<ExprAST> Step,
             std::unique_ptr<ExprAST> Body)
    : VarName(std::move(VarName)), Start(std::move(Start)), End(std::move(End)),
      Step(std::move(Step)), Body(std::move(Body)) {}

  llvm::Value *codegen() override;
};