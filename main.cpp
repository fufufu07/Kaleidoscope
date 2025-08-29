#include <KaleidoscopeJIT.h>
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include <llvm/TargetParser/Host.h>
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/IR/LegacyPassManager.h"

#include <print>



#include "AST.h"
#include "Debug.h"
#include "Lexer.h"
#include "Parser.h"

// Forward declarations for handlers
void HandleDefinition();
void HandleExtern();
void HandleTopLevelExpression();
void InitializeModuleAndManagers();

extern std::unique_ptr<llvm::orc::KaleidoscopeJIT> the_jit;
extern llvm::ExitOnError exit_on_err;
extern std::unique_ptr<llvm::Module> the_module;


/// HandleDefinition - Handle function definitions
void HandleDefinition() {
  if (auto fn_ast = ParseDefinition()) {
    std::println(stderr, "Parsed a function definition.");
  } else {
    // Skip token for error recovery.
    get_next_token();
  }
}

/// HandleExtern - Handle external declarations
void HandleExtern() {
  if (auto proto_ast = ParseExtern()) {
    std::println(stderr, "Parsed an extern.");
  } else {
    // Skip token for error recovery.
    get_next_token();
  }
}
/// WrapAsTopLevel - Wrap an expression as a top-level function
std::unique_ptr<FunctionAST> parse_top_level_expr() {
  if (auto expr = ParseExpression()) {
    return WrapAsTopLevel(std::move(expr));
  }
  return nullptr;
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
  while (true) {
    std::print(stderr, "ready> ");
    switch (get_current_token()) {
      case Token::k_tok_eof:
        return;
      case static_cast<Token>(';'):  // ignore top-level semicolons.
        get_next_token();
        break;
      case Token::k_tok_def:
        HandleDefinition();
        break;
      case Token::k_tok_extern:
        HandleExtern();
        break;
      default:
        HandleTopLevelExpression();
        break;
    }
  }
}

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X) {
  fputc((char)X, stderr);
  return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}

int main() {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  // Initialize binary operator precedence.
  InitializeBinopPrecedence();

  get_next_token();

  // Initialize the LLVM context, module, and builder.
  the_jit = exit_on_err(llvm::orc::KaleidoscopeJIT::Create());

  InitializeModuleAndManagers();

  // Add the current debug info version into the module.
  the_module->addModuleFlag(llvm::Module::Warning, "Debug Info Version",
                           llvm::DEBUG_METADATA_VERSION);

  // Darwin only supports dwarf2.
  if (llvm::Triple(llvm::sys::getProcessTriple()).isOSDarwin())
    the_module->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 2);

  // Construct the DIBuilder, we do this here because we need the module.
  DBuilder = std::make_unique<llvm::DIBuilder>(*the_module);

  // Create the compile unit for the module.
  // Currently down as "fib.ks" as a filename since we're redirecting stdin
  // but we'd like actual source locations.
  KSDbgInfo.TheCU = DBuilder->createCompileUnit(
      llvm::dwarf::DW_LANG_C, DBuilder->createFile("fib.ks", "."),
      "Kaleidoscope Compiler", false, "", 0);

  std::print(stderr, "ready> ");
  // Prime the first token.

  // Now run the main "interpreter loop".
  MainLoop();

  DBuilder->finalize();

  // Print out all of the generated code.
  the_module->print(llvm::errs(), nullptr);

  // Initialize the target registry etc.
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  const auto TargetTriple = llvm::sys::getDefaultTargetTriple();
  the_module->setTargetTriple(llvm::Triple(TargetTriple).str());

  std::string Error;
  auto Target = llvm::TargetRegistry::lookupTarget(TargetTriple, Error);

  // Print an error and exit if we couldn't find the requested target.
  // This generally occurs if we've forgotten to initialise the
  // TargetRegistry or we have a bogus target triple.
  if (!Target) {
    llvm::errs() << Error;
    return 1;
  }

  auto CPU = "generic";
  auto Features = "";

  llvm::TargetOptions opt;
  auto TheTargetMachine = Target->createTargetMachine(
      llvm::Triple(TargetTriple).str(), CPU, Features, opt, llvm::Reloc::PIC_);

  the_module->setDataLayout(TheTargetMachine->createDataLayout());

  auto Filename = "output.o";
  std::error_code EC;
  llvm::raw_fd_ostream dest(Filename, EC, llvm::sys::fs::OF_None);

  if (EC) {
    llvm::errs() << "Could not open file: " << EC.message();
    return 1;
  }

  llvm::legacy::PassManager pass;
  auto FileType = llvm::CodeGenFileType::ObjectFile;

  if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    llvm::errs() << "TheTargetMachine can't emit a file of this type";
    return 1;
  }

  pass.run(*the_module);
  dest.flush();

  llvm::outs() << "Wrote " << Filename << "\n";

  return 0;
}
