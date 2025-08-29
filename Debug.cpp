//
// Created by Mt342 on 2025/8/29.
//

#include "Debug.h"

#include <llvm/IR/IRBuilder.h>

#include "AST.h"

extern std::unique_ptr<llvm::IRBuilder<> > builder;



llvm::DIType *DebugInfo::getDoubleTy() {
  if (DblTy)
    return DblTy;

  DblTy = DBuilder->createBasicType("double", 64, llvm::dwarf::DW_ATE_float);
  return DblTy;
}

int advance() {
  int LastChar = getchar();

  if (LastChar == '\n' || LastChar == '\r') {
    LexLoc.Line++;
    LexLoc.Col = 0;
  } else
    LexLoc.Col++;
  return LastChar;
}


void DebugInfo::emitLocation(ExprAST *AST) {
  if (!AST)
    return builder->SetCurrentDebugLocation(llvm::DebugLoc());
  llvm::DIScope *Scope;
  if (LexicalBlocks.empty())
    Scope = TheCU;
  else
    Scope = LexicalBlocks.back();
  builder->SetCurrentDebugLocation(
      llvm::DILocation::get(Scope->getContext(), AST->getLine(), AST->getCol(), Scope));
}

llvm::DISubroutineType *CreateFunctionType(unsigned NumArgs) {
  llvm::SmallVector<llvm::Metadata *, 8> EltTys;
  llvm::DIType *DblTy = KSDbgInfo.getDoubleTy();

  // Add the result type.
  EltTys.push_back(DblTy);

  for (unsigned i = 0, e = NumArgs; i != e; ++i)
    EltTys.push_back(DblTy);

  return DBuilder->createSubroutineType(DBuilder->getOrCreateTypeArray(EltTys));
}