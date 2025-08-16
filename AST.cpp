#include "AST.h"

#include <map>
#include <memory>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "LLVM IR.h"
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
llvm::Value* CallExprAST::codegen() {
  // Look up the name in the global module table.
  llvm::Function* callee_f = TheModule->getFunction(callee_);
  if (!callee_f) {
    return LogErrorV("Unknown function referenced");
  }

  // If argument mismatch error.
  if (callee_f->arg_size() != args_.size()) {
    // Check if the number of arguments matches
    return LogErrorV("Incorrect # arguments passed");
  }
  /// Generate code for each argument.
  /// We create a vector of llvm::Value* to hold the generated code for each argument.
  /// This is necessary because CreateCall expects a vector of Value pointers.
  std::vector<llvm::Value*> args_v;
  for (const auto& arg : args_) {
    args_v.push_back(arg->codegen());
    if (!args_v.back()) {
      return nullptr;
    }
  }

  return Builder->CreateCall(callee_f, args_v, "calltmp");
}

/// PrototypeAST::codegen - Code generation for function prototypes.
/// This function generates LLVM IR for a function prototype.
/// It creates a function type based on the specified return type and argument types,
/// and then creates a function with the specified name and type in the module.
/// It also sets names for all arguments in the function.
/// @return  A pointer to the LLVM Function representing the prototype, or nullptr on error.
/// PrototypeAST::codegen - Code generation for function prototypes.
llvm::Function* PrototypeAST::codegen() const {
  // Make the function type:  double(double,double) etc.
  const std::vector<llvm::Type*> doubles(args_.size(),
                                 llvm::Type::getDoubleTy(*TheContext));
  // Create a function type with the specified return type and argument types.
  llvm::FunctionType* ft =
      llvm::FunctionType::get(llvm::Type::getDoubleTy(*TheContext), doubles,
                              false);
  // Create a function with the specified name and type.
  llvm::Function* f =
      llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name_,
                             TheModule.get());
  // Set names for all arguments.
  unsigned idx = 0;
  for (auto& arg : f->args()) {
    arg.setName(args_[idx++]);
  }

  return f;
}

/// FunctionAST::codegen - Code generation for function definitions.
/// This function generates LLVM IR for a function definition.
/// @return  A pointer to the LLVM Function representing the function definition, or nullptr on error.
/// FunctionAST::codegen - Code generation for function definitions.
llvm::Function* FunctionAST::codegen() const {
  // First, check for an existing function from a previous 'extern' declaration.
  llvm::Function* the_function = TheModule->getFunction(proto_->GetName());

  // If we have not yet created the function, create it now.
  if (!the_function) {
    the_function = proto_->codegen();
    // If the function cannot be created, return nullptr.
    if (!the_function) {
      return nullptr;
    }
  }

  // If the function already exists, check if it has the same signature.
  if (!the_function->empty())
    return static_cast<llvm::Function*>(LogErrorV(
        "Function cannot be redefined."));
  // Create a new basic block to start insertion into.
  llvm::BasicBlock* bb = llvm::BasicBlock::Create(
      *TheContext, "entry", the_function);
  Builder->SetInsertPoint(bb);

  // Record the function arguments in the NamedValues map.
  NamedValues.clear();
  for (auto& arg : the_function->args()) {
    NamedValues[std::string{arg.getName()}] = &arg;
  }
  // Generate code for the function body.
  // If the body is not valid, return nullptr.
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
