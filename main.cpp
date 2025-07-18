#include <sys/stat.h>

#include <cstdlib>
#include <iostream>
#include <memory>
#include <print>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>


// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum class Token : int32_t {
  tok_eof = -1,

  // commands
  tok_def = -2,
  tok_extern = -3,

  // primary
  tok_identifier = -4,
  tok_number = -5,

  // error handling
  tok_error = -6,
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number
static int LineNum = 1;           // Track line numbers for error reporting

// Cache for keywords to avoid string comparisons
static const std::unordered_map<std::string, Token> Keywords = {
    {"def", Token::tok_def}, {"extern", Token::tok_extern}};

/// gettok - Return the next token from standard input.

static void skipWhitespace(int& LastChar) {
  while (std::isspace(LastChar)) {
    if (LastChar == '\n') {
      LineNum++;
    }
    LastChar = getchar();
  }
}

static Token parseIdentifier(int& LastChar) {
  IdentifierStr.clear();
  IdentifierStr.reserve(32);

  do {
    IdentifierStr += static_cast<char>(LastChar);
    LastChar = getchar();
  } while (std::isalnum(LastChar) || LastChar == '_');

  if (const auto it = Keywords.find(IdentifierStr); it != Keywords.end()) {
    return it->second;
  }
  return Token::tok_identifier;
}

static Token parseNumber(int& LastChar) {
  std::string NumStr;
  NumStr.reserve(32);
  bool hasDecimalPoint = false;

  do {
    if (LastChar == '.') {
      if (hasDecimalPoint) {
        // 当前正在解析的数字已经包含小数点
        std::println(stderr, "Error: Invalid number format at line {}",
                     LineNum);
        return Token::tok_error;
      }
      hasDecimalPoint = true;
    }
    NumStr += static_cast<char>(LastChar);
    LastChar = getchar();
  } while (std::isdigit(LastChar) || LastChar == '.');

  if (NumStr == ".") {
    return static_cast<Token>('.');
  }

  char* endPtr{nullptr};
  NumVal = std::strtod(NumStr.c_str(), &endPtr);

  if (*endPtr != '\0') {
    std::println(stderr, "Error: Invalid number format at line {}", LineNum);
    return Token::tok_error;
  }

  return Token::tok_number;
}

static bool skipComment(int& LastChar) {
  do {
    LastChar = getchar();
  } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

  if (LastChar == '\n') {
    LineNum++;
  }

  return LastChar != EOF;
}

static Token gettok() {
  static int LastChar = ' ';

  skipWhitespace(LastChar);

  if (std::isalpha(LastChar) || LastChar == '_') {
    return parseIdentifier(LastChar);
  }

  if (std::isdigit(LastChar) || LastChar == '.') {
    return parseNumber(LastChar);
  }

  if (LastChar == '#') {
    if (skipComment(
        LastChar)) {
      // 如果已处理完注释且未到文件末尾，则换行,递归继续处理
      return gettok();
    }
  }

  if (LastChar == EOF) {
    return Token::tok_eof; //
  }

  int ThisChar = LastChar;
  LastChar = getchar();
  return static_cast<Token>(ThisChar);
}

/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  virtual ~ExprAST() = default;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  double Val;

public:
  explicit NumberExprAST(double Val) : Val(Val) {
  }
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string Name;

public:
  explicit VariableExprAST(const std::string& Name) : Name(Name) {
  }
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;

public:
  BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                std::unique_ptr<ExprAST> RHS)
    : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {
  }
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST> > Args;

public:
  CallExprAST(const std::string& Callee,
              std::vector<std::unique_ptr<ExprAST> > Args)
    : Callee(Callee), Args(std::move(Args)) {
  }
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;

public:
  PrototypeAST(const std::string& Name, std::vector<std::string> Args)
    : Name(Name), Args(std::move(Args)) {
  }

  [[nodiscard]] const std::string& getName() const { return Name; }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  // 函数定义
  std::unique_ptr<PrototypeAST> Proto; // 函数声明
  std::unique_ptr<ExprAST> Body;       // 函数体

public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body)
    : Proto(std::move(Proto)), Body(std::move(Body)) {
  }
};

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static Token CurTok;

static Token getNextToken() { return CurTok = gettok(); }

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char* Str) {
  std::println(stderr, "Error: {}", Str);
  return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char* Str) {
  LogError(Str);
  return nullptr;
}

/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
  auto Result = std::make_unique<NumberExprAST>(NumVal);
  getNextToken(); // consume the number
  return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
  getNextToken(); // eat (.
  auto V = ParseExpression();
  if (!V) return nullptr;

  if (CurTok != Token::tok_eof && CurTok != static_cast<Token>(')')) {
    return LogError("expected ')'");
  }
  getNextToken(); // eat ).
  return V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  getNextToken();  // eat identifier.

  if (CurTok != static_cast<Token>('(')) // Simple variable ref.
    return std::make_unique<VariableExprAST>(IdName);

  // Call.
  getNextToken();  // eat (
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (CurTok != static_cast<Token>(')')) {
    while (true) {
      if (auto Arg = ParseExpression())
        Args.push_back(std::move(Arg));
      else
        return nullptr;

      if (CurTok == static_cast<Token>(')'))
        break;

      if (CurTok != static_cast<Token>(','))
        return LogError("Expected ')' or ',' in argument list");
      getNextToken();
    }
  }

  // Eat the ')'.
  getNextToken();

  return std::make_unique<CallExprAST>(IdName, std::move(Args));
}
