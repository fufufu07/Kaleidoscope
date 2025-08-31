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
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include "llvm/Transforms/Utils/Mem2Reg.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"

#include <map>
#include <memory>
#include <print>
#include <string>

#include "Parser.h"
#include "AST.h"


// 全局变量和函数声明，用于管理 LLVM IR 生成和优化的核心组件。

// LLVM 上下文对象，管理 LLVM 中的全局状态。
std::unique_ptr<llvm::LLVMContext> the_context;

// LLVM 模块对象，表示一个独立的代码单元。
std::unique_ptr<llvm::Module> the_module;

// LLVM IR 构建器，用于生成 LLVM 指令。
std::unique_ptr<llvm::IRBuilder<> > builder;

// 命名值映射，用于存储当前作用域中的变量及其对应的 LLVM alloca。
std::map<std::string, llvm::AllocaInst*> named_values;

// 自定义的 Kaleidoscope JIT，用于即时编译和执行代码。
// JIT 移除，使用简化版本
// std::unique_ptr<llvm::orc::KaleidoscopeJIT> the_jit;

// 函数级别的 Pass 管理器，用于优化函数。
std::unique_ptr<llvm::FunctionPassManager> the_fpm;

// 循环分析管理器，用于管理循环级别的分析。
std::unique_ptr<llvm::LoopAnalysisManager> the_lam;

// 函数分析管理器，用于管理函数级别的分析。
std::unique_ptr<llvm::FunctionAnalysisManager> the_fam;

// 调用图强连通分量分析管理器。
std::unique_ptr<llvm::CGSCCAnalysisManager> the_cgam;

// 模块分析管理器，用于管理模块级别的分析。
std::unique_ptr<llvm::ModuleAnalysisManager> the_mam;

// Pass 仪器回调，用于监控和调试 Pass 的执行。
std::unique_ptr<llvm::PassInstrumentationCallbacks> the_pic;

// 标准仪器化工具，用于注册和管理 Pass 仪器。
std::unique_ptr<llvm::StandardInstrumentations> the_si;

// 函数原型的映射，用于存储已声明的函数。
std::map<std::string, std::unique_ptr<PrototypeAst> > function_protos;

// 解析顶层表达式，返回一个 FunctionAST。
std::unique_ptr<FunctionAST> parse_top_level_expr();

// 函数原型的 AST 类的前向声明。
class PrototypeAst;

// 全局错误处理器，用于处理 LLVM 中的错误。
llvm::ExitOnError exit_on_err;

// 获取下一个词法单元的函数。
int get_next_token();

///
/// InitializeModuleAndManagers - Initializes the LLVM module and various analysis managers.
/// This function sets up the LLVM context, module, IR builder, and various pass managers.
/// It also registers the necessary analysis passes and adds transform passes to the function pass manager.
///
void InitializeModuleAndManagers() {
  // Open a new context and module.
  the_context = std::make_unique<llvm::LLVMContext>();
  the_module = std::make_unique<llvm::Module>("Kaleidoscope", *the_context);
  // the_module->setDataLayout(the_jit->getDataLayout()); // JIT 移除

  // Create a new builder for the module.
  builder = std::make_unique<llvm::IRBuilder<> >(*the_context);

  // Create new pass and analysis managers.
  the_fpm = std::make_unique<llvm::FunctionPassManager>();
  the_lam = std::make_unique<llvm::LoopAnalysisManager>();
  the_fam = std::make_unique<llvm::FunctionAnalysisManager>();
  the_cgam = std::make_unique<llvm::CGSCCAnalysisManager>();
  the_mam = std::make_unique<llvm::ModuleAnalysisManager>();

  // Register the analysis passes.
  the_pic = std::make_unique<llvm::PassInstrumentationCallbacks>();
  the_si = std::make_unique<llvm::StandardInstrumentations>(*the_context,
    /*DebugLogging*/ true);
  the_si->registerCallbacks(*the_pic, the_mam.get());

  // Add transform passes.
  // 暂时注释掉优化 Pass，专注于编译成功
  /*
  // Promote allocas to registers.
  the_fpm->addPass(llvm::PromoteMemToRegPass());
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  the_fpm->addPass(llvm::InstCombinePass());
  // Reassociate expressions.
  the_fpm->addPass(llvm::ReassociatePass());
  // Eliminate Common SubExpressions.
  the_fpm->addPass(llvm::GVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  the_fpm->addPass(llvm::SimplifyCFGPass());
  */

  // Register analysis passes used in these transform passes.
  llvm::PassBuilder pb;
  pb.registerModuleAnalyses(*the_mam);
  pb.registerFunctionAnalyses(*the_fam);
  pb.crossRegisterProxies(*the_lam, *the_fam, *the_cgam, *the_mam);
}

/// HandleDefinition - Handle function definitions.
/// This function parses a function definition and adds it to the JIT.
void HandleDefinition() {
  if (auto fn_result = ParseDefinition()) {
    const auto fn_ast = std::move(*fn_result); // 提取 std::unique_ptr<FunctionAST>
    // 将函数原型添加到函数原型映射中
    if (const auto* fn_ir = fn_ast->codegen()) {
      std::print(stderr, "Read function definition:");
      fn_ir->print(llvm::errs());
      std::println(stderr, "");
      // JIT 相关代码移除
      // exit_on_err(the_jit->addModule(
      //     llvm::orc::ThreadSafeModule(std::move(the_module),
      //                                 std::move(the_context))));
      // InitializeModuleAndManagers();
    }
  } else {
    // 处理错误
    std::print(stderr, "Error parsing definition: {}\n", fn_result.error());
    get_next_token();
  }
}


/// HandleExtern - Handle external declarations.
/// This function parses an extern declaration and adds it to the function prototypes.
/// It expects the extern to be in the form of`extern <name>(<args>)
void HandleExtern() {
  if (auto ProtoResult = ParseExtern()) {
    auto proto_ast = std::move(*ProtoResult); // 提取 std::unique_ptr<PrototypeAST>
    if (const auto* fn_ir = proto_ast->codegen()) {
      std::print(stderr, "Read extern: ");
      fn_ir->print(llvm::errs());
      std::println(stderr, "");
      function_protos[proto_ast->GetName()] = std::move(proto_ast);
    }
  } else {
    // 处理错误
    std::print(stderr, "Error parsing extern: {}\n", ProtoResult.error());
    get_next_token();
  }
}

/// HandleTopLevelExpression - Handle top-level expressions.
/// This function parses a top-level expression and evaluates it as an
/// anonymous function. It adds the function to the JIT and executes it.
void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto FnAST = parse_top_level_expr()) {
    if (FnAST->codegen()) {
      // JIT 相关代码移除 - 简化版本只输出 IR
      std::print(stderr, "Parsed a top-level expr:");
      the_module->print(llvm::errs(), nullptr);
      std::println(stderr, "");
      /*
      // Create a ResourceTracker to track JIT'd memory allocated to our
      // anonymous expression -- that way we can free it after executing.
      const auto rt = the_jit->getMainJITDylib().createResourceTracker();

      // Create a new module for the anonymous expression.
      auto tsm = llvm::orc::ThreadSafeModule(std::move(the_module),
                                             std::move(the_context));
      exit_on_err(the_jit->addModule(std::move(tsm), rt));
      InitializeModuleAndManagers(); // 重新初始化模块和管理器

      // Search the JIT for the __anon_expr symbol.
      const auto expr_symbol = exit_on_err(the_jit->lookup("__anon_expr"));

      // Get the symbol's address and cast it to the right type (takes no
      // arguments, returns a double) so we can call it as a native function.
      const auto fp = expr_symbol.toPtr<double (*)()>();
      // 简化版本不执行，只输出 IR
      */
    }
  } else {
    // Skip token for error recovery.
    get_next_token();
  }
}

/// @brief Retrieves a function by name from the current module or function prototypes.
/// @param name The name of the function to retrieve.
/// @return  A pointer to the LLVM Function if it exists, or nullptr if it does not.
llvm::Function* get_function(const std::string& name) {
  // First, see if the function has already been added to the current module.
  if (auto* F = the_module->getFunction(name)) {
    return F;
  }

  // If not, check whether we can codegen the declaration from some existing
  // prototype.
  if (const auto fi = function_protos.find(name); fi != function_protos.end()) {
    return fi->second->codegen();
  }

  // If no existing prototype exists, return null.
  return nullptr;
}
