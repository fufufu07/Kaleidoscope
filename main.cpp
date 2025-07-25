#include <sys/stat.h>

#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <print>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum class Token : int32_t {
  kTokEof = -1,

  // commands
  kTokDef = -2,
  kTokExtern = -3,

  // primary
  kTokIdentifier = -4,
  kTokNumber = -5,

  // error handling
  kTokError = -6,
};

static std::string identifier_str;  // Filled in if tok_identifier
static double num_val;              // Filled in if tok_number
static int line_num = 1;            // Track line numbers for error reporting

// Cache for keywords to avoid string comparisons
static const std::unordered_map<std::string, Token> kKeywords = {
    {"def", Token::kTokDef}, {"extern", Token::kTokExtern}};

/// gettok - Return the next token from standard input.

static void SkipWhitespace(int& last_char) noexcept {
  while (std::isspace(last_char)) {
    if (last_char == '\n') {
      ++line_num;
    }
    last_char = getchar();
  }
}

static Token ParseIdentifier(int& last_char) {
  identifier_str.clear();
  identifier_str.reserve(32);

  do {
    identifier_str += static_cast<char>(last_char);
    last_char = getchar();
  } while (std::isalnum(last_char) || last_char == '_');

  if (const auto it = kKeywords.find(identifier_str); it != kKeywords.end()) {
    return it->second;
  }
  return Token::kTokIdentifier;
}

static Token ParseNumber(int& last_char) {
  std::string num_str;
  num_str.reserve(32);
  bool has_decimal_point = false;

  do {
    if (last_char == '.') {
      if (has_decimal_point) {
        // 当前正在解析的数字已经包含小数点
        std::println(stderr, "Error: Invalid number format at line {}",
                     line_num);
        return Token::kTokError;
      }
      has_decimal_point = true;
    }
    num_str += static_cast<char>(last_char);
    last_char = getchar();
  } while (std::isdigit(last_char) || last_char == '.');

  if (num_str == ".") {
    return static_cast<Token>('.');
  }

  char* end_ptr = nullptr;
  num_val = std::strtod(num_str.c_str(), &end_ptr);

  if (*end_ptr != '\0') {
    std::println(stderr, "Error: Invalid number format at line {}", line_num);
    return Token::kTokError;
  }

  return Token::kTokNumber;
}

static bool SkipComment(int& last_char) noexcept {
  do {
    last_char = getchar();
  } while (last_char != EOF && last_char != '\n' && last_char != '\r');

  if (last_char == '\n') {
    ++line_num;
  }

  return last_char != EOF;
}

static Token GetTok() {
  static int last_char = ' ';

  SkipWhitespace(last_char);

  if (std::isalpha(last_char) || last_char == '_') {
    return ParseIdentifier(last_char);
  }

  if (std::isdigit(last_char) || last_char == '.') {
    return ParseNumber(last_char);
  }

  if (last_char == '#') {
    if (SkipComment(last_char)) {
      // 如果已处理完注释且未到文件末尾，则递归继续处理
      return GetTok();
    }
  }

  if (last_char == EOF) {
    return Token::kTokEof;
  }

  const int this_char = last_char;
  last_char = getchar();
  return static_cast<Token>(this_char);
}

/// ExprAST - Base class for all expression nodes.
class ExprAST {
 public:
  virtual ~ExprAST() = default;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  double val_;

 public:
  explicit NumberExprAST(double val) noexcept : val_(val) {}

  [[nodiscard]] double GetValue() const noexcept { return val_; }
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string name_;

 public:
  explicit VariableExprAST(std::string name) : name_(std::move(name)) {}

  [[nodiscard]] const std::string& GetName() const noexcept { return name_; }
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  Token op_;
  std::unique_ptr<ExprAST> lhs_, rhs_;

 public:
  BinaryExprAST(Token op, std::unique_ptr<ExprAST> lhs,
                std::unique_ptr<ExprAST> rhs) noexcept
      : op_(op), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {}

  [[nodiscard]] Token GetOp() const noexcept { return op_; }
  [[nodiscard]] const ExprAST* GetLHS() const noexcept { return lhs_.get(); }
  [[nodiscard]] const ExprAST* GetRHS() const noexcept { return rhs_.get(); }
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string callee_;
  std::vector<std::unique_ptr<ExprAST>> args_;

 public:
  CallExprAST(std::string callee, std::vector<std::unique_ptr<ExprAST>> args)
      : callee_(std::move(callee)), args_(std::move(args)) {}

  [[nodiscard]] const std::string& GetCallee() const noexcept {
    return callee_;
  }
  [[nodiscard]] const std::vector<std::unique_ptr<ExprAST>>& GetArgs()
      const noexcept {
    return args_;
  }
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAST {
  std::string name_;
  std::vector<std::string> args_;

 public:
  PrototypeAST(std::string name, std::vector<std::string> args)
      : name_(std::move(name)), args_(std::move(args)) {}

  [[nodiscard]] const std::string& GetName() const noexcept { return name_; }
  [[nodiscard]] const std::vector<std::string>& GetArgs() const noexcept {
    return args_;
  }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> proto_;  // 函数声明
  std::unique_ptr<ExprAST> body_;        // 函数体

 public:
  FunctionAST(std::unique_ptr<PrototypeAST> proto,
              std::unique_ptr<ExprAST> body) noexcept
      : proto_(std::move(proto)), body_(std::move(body)) {}

  [[nodiscard]] const PrototypeAST* GetProto() const noexcept {
    return proto_.get();
  }
  [[nodiscard]] const ExprAST* GetBody() const noexcept { return body_.get(); }
};

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static Token cur_tok;

static Token GetNextToken() { return cur_tok = GetTok(); }

/// LogError* - These are little helper functions for error handling.
static std::unique_ptr<ExprAST> LogError(std::string_view str) {
  std::println(stderr, "Error: {}", str);
  return nullptr;
}

static std::unique_ptr<PrototypeAST> LogErrorP(std::string_view str) {
  std::println(stderr, "Error: {}", str);
  return nullptr;
}

/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
  auto result = std::make_unique<NumberExprAST>(num_val);
  GetNextToken();  // consume the number
  return result;
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
  GetNextToken();  // eat (.
  auto expr = ParseExpression();
  if (!expr) {
    return nullptr;
  }

  if (cur_tok != static_cast<Token>(')')) {
    return LogError("expected ')'");
  }
  GetNextToken();  // eat ).
  return expr;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string id_name{identifier_str};  // Remember the identifier name

  GetNextToken();  // eat identifier.

  if (cur_tok != static_cast<Token>('(')) {  // Simple variable ref.
    return std::make_unique<VariableExprAST>(std::move(id_name));
  }

  // Call.
  GetNextToken();                              // eat (
  std::vector<std::unique_ptr<ExprAST>> args;  // Argument expressions.

  if (cur_tok !=
      static_cast<Token>(')')) {  // If the next token is not ')', we have to
    while (true) {                // Parse the arguments.
      if (auto arg = ParseExpression()) {
        args.push_back(std::move(arg));
      } else {
        return nullptr;
      }

      if (cur_tok == static_cast<Token>(')')) {
        break;
      }

      if (cur_tok != static_cast<Token>(',')) {
        return LogError("Expected ')' or ',' in argument list");
      }
      GetNextToken();  // eat the ','
    }
  }

  // Eat the ')'.
  GetNextToken();

  return std::make_unique<CallExprAST>(std::move(id_name), std::move(args));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
  switch (cur_tok) {
    case Token::kTokIdentifier:
      return ParseIdentifierExpr();
    case Token::kTokNumber:
      return ParseNumberExpr();
    case static_cast<Token>('('):
      return ParseParenExpr();
    default:
      return LogError("unknown token when expecting an expression");
  }
}

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<Token, int> binop_precedence{};

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
  if (!isascii(cur_tok)) return -1;

  // Make sure it's a declared binop.
  const int tok_precedence = binop_precedence[cur_tok];
  if (tok_precedence <= 0) {
    return -1;
  }
  return tok_precedence;
}
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS);
/// expression
///   ::= primary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParsePrimary();
  if (!LHS) return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}

/// binoprhs
///   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS) {
  // If this is a binop, find its precedence.
  while (true) {
    const int tok_prec =
        GetTokPrecedence();  // Get the precedence of the current token.

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (tok_prec < ExprPrec) {
      return LHS;
    }
    // Okay, we know this is a binop.
    Token bin_op = cur_tok;
    GetNextToken();  // eat binop

    // Parse the primary expression after the binary operator.
    auto rhs{ParsePrimary()};
    if (!rhs) {
      return nullptr;
    }
    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    if (const int next_prec{GetTokPrecedence()}; tok_prec < next_prec) {
      if (auto pending_rhs = ParsePrimary()) {
        rhs = ParseBinOpRHS(tok_prec + 1, std::move(rhs));
        if (!rhs) {
          return nullptr;
        }
      }
    }

    // Merge LHS/RHS.
    LHS =
        std::make_unique<BinaryExprAST>(bin_op, std::move(LHS), std::move(rhs));
  }  // loop around to the top of the while loop.
}
int main() {
  // Install standard binary operators.
  // 1 is lowest precedence.
  binop_precedence[static_cast<Token>('<')] = 10;
  binop_precedence[static_cast<Token>('+')] = 20;
  binop_precedence[static_cast<Token>('-')] = 20;
  binop_precedence[static_cast<Token>('*')] = 40;  // highest.
}
