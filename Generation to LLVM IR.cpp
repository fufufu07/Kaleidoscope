#include "Generation to LLVM IR.h"

#include <map>
#include <memory>
#include "AST.h"
#include "Parser.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

// 全局静态变量，用于支持 LLVM IR 的生成和管理

// LLVMContext 是 LLVM 的核心类，用于存储与编译相关的全局信息（如类型和常量）。
// 使用 std::unique_ptr 确保内存的自动管理。
static std::unique_ptr<llvm::LLVMContext> TheContext;

// IRBuilder 是一个帮助类，用于简化 LLVM IR 指令的生成。
// 它提供了方便的接口来创建常见的指令（如算术运算、函数调用等）。
static std::unique_ptr<llvm::IRBuilder<>> Builder;

// Module 表示一个完整的代码单元（通常对应一个源文件）。
// 它是生成和优化 LLVM IR 的核心容器，包含函数和全局变量等信息。
static std::unique_ptr<llvm::Module> TheModule;

// 映射表，用于将变量名（字符串）映射到对应的 LLVM Value 对象。
// Value 是 LLVM 中的基类，表示 IR 中的一个值（如变量、常量等）。
// NamedValues 通常用于在代码生成过程中查找变量的值。
static std::map<std::string, llvm::Value *> NamedValues;

llvm::Value *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}

llvm::Value *NumberExprAST::codegen() {
  return llvm::ConstantFP::get(*TheContext, llvm::APFloat(val_));
}

llvm::Value *BinaryExprAST::codegen() {
  llvm::Value *l = lhs_->codegen();
  llvm::Value *r = rhs_->codegen();
  if (!l || !r)
    return nullptr;

  switch (op_) {
    case '+':
      return Builder->CreateFAdd(l, r, "addtmp");
    case '-':
      return Builder->CreateFSub(l, r, "subtmp");
    case '*':
      return Builder->CreateFMul(l, r, "multmp");
    case '<':
      l = Builder->CreateFCmpULT(l, r, "cmptmp");
      // Convert bool 0/1 to double 0.0 or 1.0
      return Builder->CreateUIToFP(l, llvm::Type::getDoubleTy(*TheContext),
                                   "booltmp");
    default:
      return LogErrorV("invalid binary operator");
  }
}
