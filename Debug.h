//
// Created by Mt342 on 2025/8/29.
//

#ifndef KALEIDOSCOPE_DEBUG_H
#define KALEIDOSCOPE_DEBUG_H

#include <memory>

#include "AST.h"
#include "llvm/IR/DIBuilder.h"

inline std::unique_ptr<llvm::DIBuilder> DBuilder;

inline struct DebugInfo {
	llvm::DICompileUnit *TheCU;
	llvm::DIType *DblTy;
	std::vector<llvm::DIScope *> LexicalBlocks;

	llvm::DIType *getDoubleTy();

	void emitLocation(ExprAST *AST);
} KSDbgInfo;

struct SourceLocation {
	int Line;
	int Col;
};

inline  SourceLocation CurLoc;
inline SourceLocation LexLoc{1, 0};

#endif //KALEIDOSCOPE_DEBUG_H