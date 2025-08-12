#include "AST.h"

#include <map>
#include <memory>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

// 全局静态变量，用于支持 LLVM IR 的生成和管理

// LLVMContext 是 LLVM 的核心类，用于存储与编译相关的全局信息（如类型和常量）。
// 使用 std::unique_ptr 确保内存的自动管理。
std::unique_ptr<llvm::LLVMContext> TheContext;

// IRBuilder 是一个帮助类，用于简化 LLVM IR 指令的生成。
// 它提供了方便的接口来创建常见的指令（如算术运算、函数调用等）。
std::unique_ptr<llvm::IRBuilder<> > Builder;

// Module 表示一个完整的代码单元（通常对应一个源文件）。
// 它是生成和优化 LLVM IR 的核心容器，包含函数和全局变量等信息。
std::unique_ptr<llvm::Module> TheModule;

// 映射表，用于将变量名（字符串）映射到对应的 LLVM Value 对象。
// Value 是 LLVM 中的基类，表示 IR 中的一个值（如变量、常量等）。
// NamedValues 通常用于在代码生成过程中查找变量的值。
std::map<std::string, llvm::Value*> NamedValues;
#include "Parser.h"

llvm::Value* LogErrorV(const char* Str) {
  LogError(Str);
  return nullptr;
}

/// LogError - Helper function to log errors and return nullptr.
llvm::Value* NumberExprAST::codegen() {
  return llvm::ConstantFP::get(*TheContext, llvm::APFloat(val_));
}

///  VariableExprAST::codegen - Code generation for variable expressions.
/// @return A pointer to the LLVM Value representing the variable, or nullptr on error.
llvm::Value* VariableExprAST::codegen() {
  // Look this variable up in the function.
  llvm::Value* v = NamedValues[name_]; // 查找变量名对应的 LLVM Value
  if (!v)
    LogErrorV("Unknown variable name");
  return v;
}

/// BinaryExprAST::codegen - Code generation for binary expressions.
/// @return A pointer to the LLVM Value representing the result of the binary operation, or nullptr on error.
llvm::Value* BinaryExprAST::codegen() {
  llvm::Value* l = lhs_->codegen();
  llvm::Value* r = rhs_->codegen();
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

/// CallExprAST::codegen - Code generation for function calls.
/// This function generates LLVM IR for a function call expression.
/// It looks up the function by name, checks the number of arguments, and generates
/// the call instruction with the provided arguments.
/// @return A pointer to the LLVM Value representing the function call, or nullptr on error.
/// If the function is not found or the number of arguments does not match, it logs an error.
/// If the function call is successful, it returns the generated call instruction.
/// The function expects the callee name and a vector of argument expressions.
/// It retrieves the function from the module, checks the argument count, and generates the call.
llvm::Value *CallExprAST::codegen() {
  // Look up the name in the global module table.
  llvm::Function *callee_f = TheModule->getFunction(callee_);
  if (!callee_f) {
    return LogErrorV("Unknown function referenced");
  }

  // If argument mismatch error.
  if (callee_f->arg_size() != args_.size()) {// Check if the number of arguments matches
    return LogErrorV("Incorrect # arguments passed");
  }
  /// Generate code for each argument.
  /// We create a vector of llvm::Value* to hold the generated code for each argument.
  /// This is necessary because CreateCall expects a vector of Value pointers.
  std::vector<llvm::Value *> args_v;
  for (const auto & arg : args_) {
    args_v.push_back(arg->codegen());
    if (!args_v.back()) {
      return nullptr;
    }
  }

  return Builder->CreateCall(callee_f, args_v, "calltmp");
}

llvm::Function* PrototypeAST::codegen() const {
  // Make the function type:  double(double,double) etc.
  std::vector<llvm::Type*> Doubles(args_.size(),
                                   llvm::Type::getDoubleTy(*TheContext));
  llvm::FunctionType* FT =
      llvm::FunctionType::get(llvm::Type::getDoubleTy(*TheContext), Doubles,
                              false);

  llvm::Function* F =
      llvm::Function::Create(FT, llvm::Function::ExternalLinkage, name_,
                             TheModule.get());
  // Set names for all arguments.
  unsigned idx = 0;
  for (auto& arg : F->args()) {
    arg.setName(args_[idx++]);
  }

  return F;
}

llvm::Function *FunctionAST::codegen() {
  // First, check for an existing function from a previous 'extern' declaration.
  llvm::Function* the_function = TheModule->getFunction(proto_->GetName());

  if (!the_function)
    the_function = proto_->codegen();

  if (!the_function)
    return nullptr;

  if (!the_function->empty())
    return static_cast<llvm::Function*>(LogErrorV("Function cannot be redefined."));
  // Create a new basic block to start insertion into.
  llvm::BasicBlock* bb = llvm::BasicBlock::Create(*TheContext, "entry", the_function);
  Builder->SetInsertPoint(bb);

  // Record the function arguments in the NamedValues map.
  NamedValues.clear();
  for (auto& arg : the_function->args())
    NamedValues[std::string(arg.getName())] = &arg;
  if (llvm::Value* ret_val = body_->codegen()) {
    // Finish off the function.
    Builder->CreateRet(ret_val);

    // Validate the generated code, checking for consistency.
    verifyFunction(*the_function);

    return the_function;
  }
  // Error reading body, remove function.
  the_function->eraseFromParent();
  return nullptr;
}
