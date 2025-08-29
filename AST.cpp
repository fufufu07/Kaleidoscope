#include "AST.h"

#include <llvm/Analysis/LoopAnalysisManager.h>

#include <map>
#include <memory>

#include "Debug.h"
#include "Lexer.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "Parser.h"

extern std::unique_ptr<llvm::LLVMContext> the_context;
extern std::unique_ptr<llvm::Module> the_module;
extern std::unique_ptr<llvm::IRBuilder<> > builder;
extern std::map<std::string, llvm::AllocaInst*> named_values;
extern std::unique_ptr<llvm::FunctionPassManager> the_fpm;
extern std::unique_ptr<llvm::FunctionAnalysisManager> the_fam;
extern std::map<std::string, std::unique_ptr<PrototypeAst> > function_protos;

llvm::Function *get_function(std::string name);
std::unique_ptr<ExprAST> ParseIfExpr();
std::unique_ptr<ExprAST> ParseForExpr();
llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *TheFunction,
                                                llvm::StringRef VarName);
llvm::DISubroutineType *CreateFunctionType(unsigned NumArgs);

extern Token cur_tok;
extern std::map<Token, int> binop_precedence;

llvm::Value* LogErrorV(const char* Str) {
  LogError(Str);
  return nullptr;
}

/// LogError - Helper function to log errors and return nullptr.
llvm::Value* NumberExprAST::codegen() {
  KSDbgInfo.emitLocation(this);
  return llvm::ConstantFP::get(*the_context, llvm::APFloat(val_));
}

///  VariableExprAST::codegen - Code generation for variable expressions.
/// @return A pointer to the LLVM Value representing the variable, or nullptr on error.
llvm::Value* VariableExprAST::codegen() {
  // Look this variable up in the function.
  llvm::AllocaInst* A = named_values[name_]; // 查找变量名对应的 LLVM Value
  if (!A) {
    LogErrorV("Unknown variable name");
  }
  KSDbgInfo.emitLocation(this);
  // Load the value.
  return builder->CreateLoad(A->getAllocatedType(), A, name_.c_str());
}

/// BinaryExprAST::codegen - Code generation for binary expressions.
/// @return A pointer to the LLVM Value representing the result of the binary operation, or nullptr on error.
llvm::Value* BinaryExprAST::codegen() {
  KSDbgInfo.emitLocation(this);

  // Special case '=' because we don't want to emit the LHS as an expression.
  if (op_ == static_cast<Token>('=')) {
    // Assignment requires the LHS to be an identifier.
    // This assume we're building without RTTI because LLVM builds that way by
    // default.  If you build LLVM with RTTI this can be changed to a
    // dynamic_cast for automatic error checking.
    auto *LHSE = static_cast<VariableExprAST *>(lhs_.get());
    if (!LHSE) {
      return LogErrorV("destination of '=' must be a variable");
    }
    // Codegen the RHS.
    llvm::Value *Val = rhs_->codegen();
    if (!Val) {
      return nullptr;
    }

    // Look up the name.
    llvm::Value *Variable = named_values[LHSE->GetName()];
    if (!Variable) {
      return LogErrorV("Unknown variable name");
    }

    builder->CreateStore(Val, Variable);
    return Val;
  }

  llvm::Value *L = lhs_->codegen();
  llvm::Value *R = rhs_->codegen();
  if (!L || !R)
    return nullptr;

  switch (op_) {
    case '+':
      return builder->CreateFAdd(L, R, "addtmp");
    case '-':
      return builder->CreateFSub(L, R, "subtmp");
    case '*':
      return builder->CreateFMul(L, R, "multmp");
    case '<':
      L = builder->CreateFCmpULT(L, R, "cmptmp");
      // Convert bool 0/1 to double 0.0 or 1.0
      return builder->CreateUIToFP(L, llvm::Type::getDoubleTy(*the_context), "booltmp");
    default:
      break;
  }

  // If it wasn't a builtin binary operator, it must be a user defined one. Emit
  // a call to it.
  llvm::Function *F = get_function(std::string("binary") + std::to_string(static_cast<int>(op_)));
  assert(F && "binary operator not found!");

  llvm::Value *Ops[] = {L, R};
  return builder->CreateCall(F, Ops, "binop");
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
  KSDbgInfo.emitLocation(this);

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
  auto &p = *proto_;
  function_protos[proto_->GetName()] = std::move(proto_);
  llvm::Function *the_function = get_function(p.GetName());
  if (!the_function) {
    return nullptr;
  }

  // If this is an operator, install it.
  if (p.is_binary_op())
    binop_precedence[static_cast<Token>(p.get_operator_name())] = p.get_binary_precedence();

  // Create a new basic block to start insertion into.
  llvm::BasicBlock *bb = llvm::BasicBlock::Create(*the_context, "entry", the_function);
  builder->SetInsertPoint(bb);

  // Create a subprogram DIE for this function.
  llvm::DIFile *Unit = DBuilder->createFile(KSDbgInfo.TheCU->getFilename(),
                                            KSDbgInfo.TheCU->getDirectory());
  llvm::DIScope *FContext = Unit;
  unsigned LineNo = p.GetName().length();
  unsigned ScopeLine = LineNo;
  llvm::DISubprogram *SP = DBuilder->createFunction(
      FContext, p.GetName(), llvm::StringRef(), Unit, LineNo,
      CreateFunctionType(the_function->arg_size()), ScopeLine,
      llvm::DINode::FlagPrototyped, llvm::DISubprogram::SPFlagDefinition);
  the_function->setSubprogram(SP);

  // Push the current scope.
  KSDbgInfo.LexicalBlocks.push_back(SP);

  // Unset the location for the prologue emission (leading instructions with no
  // location in a function are considered part of the prologue and the debugger
  // will run past them when breaking on a function)
  KSDbgInfo.emitLocation(nullptr);

  // Record the function arguments in the NamedValues map.
  named_values.clear();
  unsigned ArgIdx = 0;
  for (auto &Arg : the_function->args()) {
    // Create an alloca for this variable.
    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(the_function, Arg.getName());

    // Create a debug descriptor for the variable.
    llvm::DILocalVariable *D = DBuilder->createParameterVariable(
        SP, Arg.getName(), ++ArgIdx, Unit, LineNo, KSDbgInfo.getDoubleTy(),
        true);

    DBuilder->insertDeclare(Alloca, D, DBuilder->createExpression(),
                            llvm::DILocation::get(SP->getContext(), LineNo, 0, SP),
                            builder->GetInsertBlock());

    // Store the initial value into the alloca.
    builder->CreateStore(&Arg, Alloca);

    // Add arguments to variable symbol table.
    named_values[std::string(Arg.getName())] = Alloca;
  }

  KSDbgInfo.emitLocation(body_.get());

  if (llvm::Value *ret_val = body_->codegen()) {
    // Finish off the function.
    builder->CreateRet(ret_val);

    // Pop off the lexical block for the function.
    KSDbgInfo.LexicalBlocks.pop_back();

    // Validate the generated code, checking for consistency.
    llvm::verifyFunction(*the_function);

    return the_function;
  }

  // Error reading body, remove function.
  the_function->eraseFromParent();

  if (p.is_binary_op())
    binop_precedence.erase(static_cast<Token>(p.get_operator_name()));

  // Pop off the lexical block for the function since we added it
  // unconditionally.
  KSDbgInfo.LexicalBlocks.pop_back();

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
  KSDbgInfo.emitLocation(this);

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

/// ForExprAST::codegen - Code generation for for/in expressions.
/// This function generates LLVM IR for a for loop expression.
/// It sets up the loop variable, start, end, and step expressions,
/// and generates the loop body. It uses basic blocks to handle the loop control flow.
/// @return A pointer to the LLVM Value representing the result of the for expression,
/// or nullptr on error.
llvm::Value* ForExprAST::codegen() {

  llvm::Function *TheFunction = builder->GetInsertBlock()->getParent();

  // Create an alloca for the variable in the entry block.
  llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);

  KSDbgInfo.emitLocation(this);

  // Emit the start code first, without 'variable' in scope.
  llvm::Value *StartVal = Start->codegen();
  if (!StartVal)
    return nullptr;

  // Store the value into the alloca.
  builder->CreateStore(StartVal, Alloca);

  // Make the new basic block for the loop header, inserting after current
  // block.
llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(*the_context, "loop", TheFunction);

  // Insert an explicit fall through from the current block to the LoopBB.
  builder->CreateBr(LoopBB);

  // Start insertion in LoopBB.
  builder->SetInsertPoint(LoopBB);

  // Within the loop, the variable is defined equal to the PHI node.  If it
  // shadows an existing variable, we have to restore it, so save it now.
llvm::AllocaInst *OldVal = named_values[VarName];
  named_values[VarName] = Alloca;

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

  // Compute the end condition.
llvm::Value *EndCond = End->codegen();
  if (!EndCond)
    return nullptr;

  // Reload, increment, and restore the alloca.  This handles the case where
  // the body of the loop mutates the variable.
llvm::Value *CurVar =
      builder->CreateLoad(Alloca->getAllocatedType(), Alloca, VarName.c_str());
llvm::Value *NextVar = builder->CreateFAdd(CurVar, StepVal, "nextvar");
  builder->CreateStore(NextVar, Alloca);

  // Convert condition to a bool by comparing non-equal to 0.0.
  EndCond = builder->CreateFCmpONE(
      EndCond, llvm::ConstantFP::get(*the_context, llvm::APFloat(0.0)), "loopcond");

  // Create the "after loop" block and insert it.
llvm::BasicBlock *AfterBB =
    llvm::BasicBlock::Create(*the_context, "afterloop", TheFunction);

  // Insert the conditional branch into the end of LoopEndBB.
  builder->CreateCondBr(EndCond, LoopBB, AfterBB);

  // Any new code will be inserted in AfterBB.
  builder->SetInsertPoint(AfterBB);

  // Restore the unshadowed variable.
  if (OldVal)
    named_values[VarName] = OldVal;
  else
    named_values.erase(VarName);

  // for expr always returns 0.0.
  return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(*the_context));;
}

llvm::Value* UnaryExprAST::codegen() {
  llvm::Value *OperandV = Operand->codegen();
  if (!OperandV)
    return nullptr;

  llvm::Function *f = get_function(std::string("unary") + Opcode);
  if (!f)
    return LogErrorV("Unknown unary operator");
  KSDbgInfo.emitLocation(this);
  return builder->CreateCall(f, OperandV, "unop");

}

llvm::Value* VarExprAST::codegen() {
  std::vector<llvm::AllocaInst *> OldBindings;

  llvm::Function *TheFunction = builder->GetInsertBlock()->getParent();

  // Register all variables and emit their initializer.
  for (auto & [fst, snd] : VarNames) {
    const std::string &VarName = fst;
    ExprAST *Init = snd.get();
    // Emit the initializer before adding the variable to scope, this prevents
    // the initializer from referencing the variable itself, and permits stuff
    // like this:
    //  var a = 1 in
    //    var a = a in ...   # refers to outer 'a'.
    llvm::Value *InitVal;
    if (Init) {
      InitVal = Init->codegen();
      if (!InitVal)
        return nullptr;
    } else { // If not specified, use 0.0.
      InitVal = llvm::ConstantFP::get(*the_context, llvm::APFloat(0.0));
    }

    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
    builder->CreateStore(InitVal, Alloca);

    // Remember the old variable binding so that we can restore the binding when
    // we unrecurse.
    OldBindings.push_back(named_values[VarName]);

    // Remember this binding.
    named_values[VarName] = Alloca;

    KSDbgInfo.emitLocation(this);

    // Codegen the body, now that all vars are in scope.
    llvm::Value *BodyVal = Body->codegen();
    if (!BodyVal)
      return nullptr;
    // Pop all our variables from scope.
    for (unsigned i = 0, e = VarNames.size(); i != e; ++i)
      named_values[VarNames[i].first] = OldBindings[i];

    // Return the body computation.
    return BodyVal;
  }
}

std::unique_ptr<ExprAST> ParseVarExpr();

static std::unique_ptr<ExprAST> ParsePrimary() {
  switch (cur_tok) {
    default:
      return LogError("unknown token when expecting an expression");
    case Token::k_tok_identifier:
      return ParseIdentifierExpr();
    case Token::k_tok_number:
      return ParseNumberExpr();
    case '(':
      return ParseParenExpr();
    case Token::k_tok_if:
      return ParseIfExpr();
    case Token::k_tok_for:
      return ParseForExpr();
    case Token::k_tok_var:
      return ParseVarExpr();
  }
}

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
/// the function.  This is used for mutable variables etc.
llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *TheFunction,
                                                llvm::StringRef VarName) {
  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                         TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(llvm::Type::getDoubleTy(*the_context), nullptr,
                           VarName);
}

std::unique_ptr<ExprAST> ParseVarExpr() {
  get_next_token();  // eat the var.

  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;

  // At least one variable name is required.
  if (cur_tok != Token::k_tok_identifier)
    return LogError("expected identifier after var");
  while (true) {
    std::string Name = identifier_str;
    get_next_token();  // eat identifier.

    // Read the optional initializer.
    std::unique_ptr<ExprAST> Init;
    if (cur_tok == static_cast<Token>('=')) {
      get_next_token(); // eat the '='.

      Init = ParseExpression();
      if (!Init) return nullptr;
    }

    VarNames.push_back(std::make_pair(Name, std::move(Init)));

    // End of var list, exit loop.
    if (cur_tok != static_cast<Token>(',')) break;
    get_next_token(); // eat the ','.

    if (cur_tok != Token::k_tok_identifier)
      return LogError("expected identifier list after var");
  }
  // At this point, we have to have 'in'.
  if (cur_tok != Token::k_tok_in)
    return LogError("expected 'in' keyword after 'var'");
  get_next_token();  // eat 'in'.

  auto Body = ParseExpression();
  if (!Body)
    return nullptr;

  return std::make_unique<VarExprAST>(std::move(VarNames),
                                       std::move(Body));
}