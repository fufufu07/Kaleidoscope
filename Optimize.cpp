#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Value.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/IR/PassInstrumentation.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/Error.h>
#include <KaleidoscopeJIT.h>

#include <map>
#include <memory>
#include <print>
#include <string>

#include "Parser.h"
#include "AST.h"


// 全局变量和函数声明，用于管理 LLVM IR 生成和优化的核心组件。

// LLVM 上下文对象，管理 LLVM 中的全局状态。
std::unique_ptr<llvm::LLVMContext> TheContext;

// LLVM 模块对象，表示一个独立的代码单元。
std::unique_ptr<llvm::Module> TheModule;

// LLVM IR 构建器，用于生成 LLVM 指令。
std::unique_ptr<llvm::IRBuilder<> > Builder;

// 命名值映射，用于存储当前作用域中的变量及其对应的 LLVM 值。
std::map<std::string, llvm::Value*> NamedValues;

// 自定义的 Kaleidoscope JIT，用于即时编译和执行代码。
std::unique_ptr<llvm::orc::KaleidoscopeJIT> TheJIT;

// 函数级别的 Pass 管理器，用于优化函数。
std::unique_ptr<llvm::FunctionPassManager> TheFPM;

// 循环分析管理器，用于管理循环级别的分析。
std::unique_ptr<llvm::LoopAnalysisManager> TheLAM;

// 函数分析管理器，用于管理函数级别的分析。
std::unique_ptr<llvm::FunctionAnalysisManager> TheFAM;

// 调用图强连通分量分析管理器。
std::unique_ptr<llvm::CGSCCAnalysisManager> TheCGAM;

// 模块分析管理器，用于管理模块级别的分析。
std::unique_ptr<llvm::ModuleAnalysisManager> TheMAM;

// Pass 仪器回调，用于监控和调试 Pass 的执行。
std::unique_ptr<llvm::PassInstrumentationCallbacks> ThePIC;

// 标准仪器化工具，用于注册和管理 Pass 仪器。
std::unique_ptr<llvm::StandardInstrumentations> TheSI;

// 函数原型的映射，用于存储已声明的函数。
std::map<std::string, std::unique_ptr<PrototypeAST> > FunctionProtos;

// 解析函数定义，返回一个包含 FunctionAST 的 std::expected。
std::expected<std::unique_ptr<FunctionAST>, std::string_view> ParseDefinition();

// 解析外部函数声明，返回一个包含 PrototypeAST 的 std::expected。
std::expected<std::unique_ptr<PrototypeAST>, std::string_view> ParseExtern();

// 解析顶层表达式，返回一个 FunctionAST。
std::unique_ptr<FunctionAST> ParseTopLevelExpr();

// 函数原型的 AST 类的前向声明。
class PrototypeAST;

// 全局错误处理器，用于处理 LLVM 中的错误。
extern llvm::ExitOnError ExitOnErr;

// 获取下一个词法单元的函数。
int getNextToken();


void InitializeModuleAndManagers() {
  // Open a new context and module.
  TheContext = std::make_unique<llvm::LLVMContext>();
  TheModule = std::make_unique<llvm::Module>("KaleidoscopeJIT", *TheContext);
  TheModule->setDataLayout(TheJIT->getDataLayout());

  // Create a new builder for the module.
  Builder = std::make_unique<llvm::IRBuilder<> >(*TheContext);

  // Create new pass and analysis managers.
  TheFPM = std::make_unique<llvm::FunctionPassManager>();
  TheLAM = std::make_unique<llvm::LoopAnalysisManager>();
  TheFAM = std::make_unique<llvm::FunctionAnalysisManager>();
  TheCGAM = std::make_unique<llvm::CGSCCAnalysisManager>();
  TheMAM = std::make_unique<llvm::ModuleAnalysisManager>();
  ThePIC = std::make_unique<llvm::PassInstrumentationCallbacks>();
  TheSI = std::make_unique<llvm::StandardInstrumentations>(*TheContext,
    /*DebugLogging*/ true);
  TheSI->registerCallbacks(*ThePIC, TheMAM.get());
  // Register analysis passes used in these transform passes.
  llvm::PassBuilder PB;
  PB.registerModuleAnalyses(*TheMAM);
  PB.registerFunctionAnalyses(*TheFAM);
  PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
}

void HandleDefinition() {
  auto FnResult = ParseDefinition();
  if (FnResult) {
    auto FnAST = std::move(*FnResult); // 提取 std::unique_ptr<FunctionAST>
    if (auto* FnIR = FnAST->codegen()) {
      std::print(stderr, "Read function definition:");
      FnIR->print(llvm::errs());
      std::println(stderr, "");
      ExitOnErr(TheJIT->addModule(
          llvm::orc::ThreadSafeModule(std::move(TheModule),
                                      std::move(TheContext))));
      InitializeModuleAndManagers();
    }
  } else {
    // 处理错误
    std::print(stderr, "Error parsing definition: {}\n", FnResult.error());
    getNextToken();
  }
}

void HandleExtern() {
  auto ProtoResult = ParseExtern();
  if (ProtoResult) {
    auto ProtoAST = std::move(*ProtoResult); // 提取 std::unique_ptr<PrototypeAST>
    if (auto* FnIR = ProtoAST->codegen()) {
      std::print(stderr, "Read extern: ");
      FnIR->print(llvm::errs());
      std::println(stderr, "");
      FunctionProtos[ProtoAST->GetName()] = std::move(ProtoAST);
    }
  } else {
    // 处理错误
    std::print(stderr, "Error parsing extern: {}\n", ProtoResult.error());
    getNextToken();
  }
}

void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto FnAST = ParseTopLevelExpr()) {
    if (FnAST->codegen()) {
      // Create a ResourceTracker to track JIT'd memory allocated to our
      // anonymous expression -- that way we can free it after executing.
      auto RT = TheJIT->getMainJITDylib().createResourceTracker();

      auto TSM = llvm::orc::ThreadSafeModule(std::move(TheModule),
                                             std::move(TheContext));
      ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
      InitializeModuleAndManagers();

      // Search the JIT for the __anon_expr symbol.
      auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));

      // Get the symbol's address and cast it to the right type (takes no
      // arguments, returns a double) so we can call it as a native function.
      double (*FP)() = ExprSymbol.toPtr<double (*)()>();
      std::println(stderr, "Evaluated to {}", FP());

      // Delete the anonymous expression module from the JIT.
      ExitOnErr(RT->remove());
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}
