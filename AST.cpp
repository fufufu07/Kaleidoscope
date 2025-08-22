#include "AST.h"

#include <llvm/Analysis/LoopAnalysisManager.h>

#include <map>
#include <memory>

#include "Lexer.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "Parser.h"

extern std::unique_ptr<llvm::LLVMContext> the_context;
extern std::unique_ptr<llvm::Module> the_module;
extern std::unique_ptr<llvm::IRBuilder<> > builder;
extern std::map<std::string, llvm::Value*> named_values;
extern std::unique_ptr<llvm::FunctionPassManager> the_fpm;
extern std::unique_ptr<llvm::FunctionAnalysisManager> the_fam;
extern std::map<std::string, std::unique_ptr<PrototypeAst> > function_protos;

llvm::Function *get_function(std::string name);
std::unique_ptr<ExprAST> ParseIfExpr();

extern Token cur_tok;

llvm::Value* LogErrorV(const char* Str) {
  LogError(Str);
  return nullptr;
}

/// LogError - Helper function to log errors and return nullptr.
llvm::Value* NumberExprAST::codegen() {
  return llvm::ConstantFP::get(*the_context, llvm::APFloat(val_));
}

///  VariableExprAST::codegen - Code generation for variable expressions.
/// @return A pointer to the LLVM Value representing the variable, or nullptr on error.
llvm::Value* VariableExprAST::codegen() {
  // Look this variable up in the function.
  llvm::Value* v = named_values[name_]; // 查找变量名对应的 LLVM Value
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
      return builder->CreateFAdd(l, r, "addtmp");
    case '-':
      return builder->CreateFSub(l, r, "subtmp");
    case '*':
      return builder->CreateFMul(l, r, "multmp");
    case '<':
      l = builder->CreateFCmpULT(l, r, "cmptmp");
      // Convert bool 0/1 to double 0.0 or 1.0
      return builder->CreateUIToFP(l, llvm::Type::getDoubleTy(*the_context),
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
  llvm::Function* callee_f = get_function(callee_);
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

  // Create the call instruction.
  // The CreateCall function generates the call instruction for the function.
  return builder->CreateCall(callee_f, args_v, "calltmp");
}

/// PrototypeAST::codegen - Code generation for function prototypes.
/// This function generates LLVM IR for a function prototype.
/// It creates a function type based on the specified return type and argument types,
/// and then creates a function with the specified name and type in the module.
/// It also sets names for all arguments in the function.
/// @return  A pointer to the LLVM Function representing the prototype, or nullptr on error.
/// PrototypeAST::codegen - Code generation for function prototypes.
llvm::Function* PrototypeAst::codegen() const {
  // Make the function type:  double(double,double) etc.
  const std::vector<llvm::Type*> doubles(args_.size(),
                                 llvm::Type::getDoubleTy(*the_context));
  // Create a function type with the specified return type and argument types.
  llvm::FunctionType* ft =
      llvm::FunctionType::get(llvm::Type::getDoubleTy(*the_context), doubles,
                              false);
  // Create a function with the specified name and type.
  llvm::Function* f =
      llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name_,
                             the_module.get());
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
llvm::Function* FunctionAST::codegen() {
  // Transfer ownership of the prototype to the FunctionProtos map, but keep a
  // reference to it for use below.
  const auto &p = *proto_;
  function_protos[proto_->GetName()] = std::move(proto_);
  llvm::Function *the_function = get_function(p.GetName());
  if (!the_function) {
    return nullptr;
  }

  // Create a new basic block to start insertion into.
  llvm::BasicBlock *bb = llvm::BasicBlock::Create(*the_context, "entry", the_function);
  builder->SetInsertPoint(bb);

  // Record the function arguments in the NamedValues map.
  named_values.clear();
  for (auto &arg : the_function->args()) {
    named_values[std::string(arg.getName())] = &arg;
  }

  if (llvm::Value *ret_val = body_->codegen()) {
    // Finish off the function.
    builder->CreateRet(ret_val);

    // Validate the generated code, checking for consistency.
    llvm::verifyFunction(*the_function);

    // Run the optimizer on the function.
    the_fpm->run(*the_function, *the_fam);

    return the_function;
  }

  // Error reading body, remove function.
  the_function->eraseFromParent();
  return nullptr;
}

/// IfExprAST::codegen - Code generation for if expressions.
/// This function generates LLVM IR for an if expression.
/// It evaluates the condition, and based on the result, it generates code for
/// the then and else branches. It uses basic blocks to handle the control flow.
/// @return A pointer to the LLVM Value representing the result of the if expression,
/// or nullptr on error.
/// If the condition is true, it evaluates the then branch; otherwise, it evaluates the else
/// branch. It uses PHI nodes to merge the results from both branches into a single value.
llvm::Value* IfExprAST::codegen() {
  llvm::Value *CondV = Cond->codegen();
  if (!CondV)
    return nullptr;

  // Convert condition to a bool by comparing non-equal to 0.0.
  CondV = builder->CreateFCmpONE(
      CondV, llvm::ConstantFP::get(*the_context, llvm::APFloat(0.0)), "ifcond");
  llvm::Function *TheFunction = builder->GetInsertBlock()->getParent();

  // Create blocks for the then and else cases.  Insert the 'then' block at the
  // end of the function.
  llvm::BasicBlock *ThenBB =
      llvm::BasicBlock::Create(*the_context, "then", TheFunction);
  llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(*the_context, "else");
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*the_context, "ifcont");

  builder->CreateCondBr(CondV, ThenBB, ElseBB);
  // Emit then value.
  builder->SetInsertPoint(ThenBB);
  // Codegen the 'then' value.
  // The 'then' block is where we will insert the code for the 'then'
  llvm::Value *ThenV = Then->codegen();
  if (!ThenV) {
    return nullptr;
  }

  builder->CreateBr(MergeBB);
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = builder->GetInsertBlock();

  // Now, we have to insert the 'else' block after the 'then' block.
  // Emit else block.
  TheFunction->insert(TheFunction->end(), ElseBB);
  builder->SetInsertPoint(ElseBB);

  llvm::Value *ElseV = Else->codegen();
  if (!ElseV) {
    return nullptr;
  }

  builder->CreateBr(MergeBB);
  // codegen of 'Else' can change the current block, update ElseBB for the PHI.
  ElseBB = builder->GetInsertBlock();

  // Now we have to insert the merge block after the 'else' block.
  // Emit merge block.
  TheFunction->insert(TheFunction->end(), MergeBB);
  builder->SetInsertPoint(MergeBB);
  llvm::PHINode *PN =
    builder->CreatePHI(llvm::Type::getDoubleTy(*the_context), 2, "iftmp");

  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
}

llvm::Value* ForExprAST::codegen() {
  // Emit the start code first, without 'variable' in scope.
  llvm::Value *StartVal = Start->codegen();
  if (!StartVal)
    return nullptr;
  // Make the new basic block for the loop header, inserting after current
  // block.
  llvm::Function *TheFunction = builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *PreheaderBB = builder->GetInsertBlock();
  llvm::BasicBlock *LoopBB =
      llvm::BasicBlock::Create(*the_context, "loop", TheFunction);

  // Insert an explicit fall through from the current block to the LoopBB.
  builder->CreateBr(LoopBB);
  // Start insertion in LoopBB.
  builder->SetInsertPoint(LoopBB);

  // Start the PHI node with an entry for Start.
  llvm::PHINode *Variable = builder->CreatePHI(llvm::Type::getDoubleTy(*the_context),
                                               2, VarName);
  Variable->addIncoming(StartVal, PreheaderBB);
  // Within the loop, the variable is defined equal to the PHI node.  If it
  // shadows an existing variable, we have to restore it, so save it now.
  llvm::Value *OldVal = named_values[VarName];
  named_values[VarName] = Variable;

  // Emit the body of the loop.  This, like any other expr, can change the
  // current BB.  Note that we ignore the value computed by the body, but don't
  // allow an error.
  if (!Body->codegen())
    return nullptr;
  // Emit the step value.
  llvm::Value *StepVal = nullptr;
  if (Step) {
    StepVal = Step->codegen();
    if (!StepVal)
      return nullptr;
  } else {
    // If not specified, use 1.0.
    StepVal = llvm::ConstantFP::get(*the_context, llvm::APFloat(1.0));
  }

  llvm::Value *NextVar = builder->CreateFAdd(Variable, StepVal, "nextvar");
  // Compute the end condition.
  llvm::Value *EndCond = End->codegen();
  if (!EndCond)
    return nullptr;

  // Convert condition to a bool by comparing non-equal to 0.0.
  EndCond = builder->CreateFCmpONE(
      EndCond, llvm::ConstantFP::get(*the_context, llvm::APFloat(0.0)), "loopcond");
  // Create the "after loop" block and insert it.
  llvm::BasicBlock *LoopEndBB = builder->GetInsertBlock();
  llvm::BasicBlock *AfterBB =
      llvm::BasicBlock::Create(*the_context, "afterloop", TheFunction);

  // Insert the conditional branch into the end of LoopEndBB.
  builder->CreateCondBr(EndCond, LoopBB, AfterBB);

  // Any new code will be inserted in AfterBB.
  builder->SetInsertPoint(AfterBB);  // Add a new entry to the PHI node for the backedge.
  Variable->addIncoming(NextVar, LoopEndBB);

  // Restore the unshadowed variable.
  if (OldVal)
    named_values[VarName] = OldVal;
  else
    named_values.erase(VarName);

  // for expr always returns 0.0.
  return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(*the_context));
}

/// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
static std::unique_ptr<ExprAST> ParseForExpr() {
  get_next_token();  // eat the for.

  if (cur_tok != Token::kTokIdentifier)
    return LogError("expected identifier after for");

  std::string IdName = identifier_str;
  get_next_token();  // eat identifier.

  if (cur_tok != '=')
    return LogError("expected '=' after for");
  get_next_token();  // eat '='.


  auto Start = ParseExpression();
  if (!Start)
    return nullptr;
  if (cur_tok != ',')
    return LogError("expected ',' after for start value");
  get_next_token();

  auto End = ParseExpression();
  if (!End)
    return nullptr;

  // The step value is optional.
  std::unique_ptr<ExprAST> Step;
  if (cur_tok == ',') {
    get_next_token();
    Step = ParseExpression();
    if (!Step)
      return nullptr;
  }

  if (cur_tok != Token::kTokIn)
    return LogError("expected 'in' after for");
  get_next_token();  // eat 'in'.

  auto Body = ParseExpression();
  if (!Body)
    return nullptr;

  return std::make_unique<ForExprAST>(IdName, std::move(Start),
                                       std::move(End), std::move(Step),
                                       std::move(Body));
}

static std::unique_ptr<ExprAST> ParsePrimary() {
  switch (cur_tok) {
    default:
      return LogError("unknown token when expecting an expression");
    case Token::kTokIdentifier:
      return ParseIdentifierExpr();
    case Token::kTokNumber:
      return ParseNumberExpr();
    case '(':
      return ParseParenExpr();
    case Token::kTokIf:
      return ParseIfExpr();
    case Token::kTokFor:
      return ParseForExpr();
  }
}