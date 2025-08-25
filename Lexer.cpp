#include "Lexer.h"

#include <cctype>
#include <cstdlib>
#include <iostream>
#include <print>
#include <unordered_map>

// Global lexer state
std::string identifier_str; // Filled in if tok_identifier
double num_val;             // Filled in if tok_number
int line_num = 1;           // Track line numbers for error reporting

// Cache for keywords to avoid string comparisons
static const std::unordered_map<std::string, Token> kKeywords = {
    {"def", Token::k_tok_def}, {"extern", Token::k_tok_extern}};

// Current token
Token cur_tok;

static void SkipWhitespace(int& last_char) noexcept {
  while (std::isspace(last_char)) {
    if (last_char == '\n') {
      ++line_num;
    }
    last_char = std::cin.get();
  }
}

static Token ParseIdentifier(int& last_char) {
  identifier_str.clear();
  identifier_str.reserve(32);

  do {
    identifier_str += static_cast<char>(last_char);
    last_char = std::cin.get();
  } while (std::isalnum(last_char) || last_char == '_');

  if (const auto it = kKeywords.find(identifier_str); it != kKeywords.end()) {
    return it->second;
  }
  return Token::k_tok_identifier;
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
        return Token::k_tok_error;
      }
      has_decimal_point = true;
    }
    num_str += static_cast<char>(last_char);
    last_char = std::cin.get();
  } while (std::isdigit(last_char) || last_char == '.');

  if (num_str == ".") {
    return static_cast<Token>('.');
  }

  char* end_ptr = nullptr;
  num_val = std::strtod(num_str.c_str(), &end_ptr);

  if (*end_ptr != '\0') {
    std::println(stderr, "Error: Invalid number format at line {}", line_num);
    return Token::k_tok_error;
  }

  return Token::k_tok_number;
}

static bool SkipComment(int& last_char) noexcept {
  do {
    last_char = std::cin.get();
  } while (last_char != EOF && last_char != '\n' && last_char != '\r');

  if (last_char == '\n') {
    ++line_num;
  }

  return last_char != EOF;
}

Token get_tok() {
  static int last_char = ' ';

  // Skip any whitespace.
  while (isspace(last_char)) {
    last_char = getchar();
  }

  if (isalpha(last_char)) {
    // identifier: [a-zA-Z][a-zA-Z0-9]*
    identifier_str = last_char;
    while (isalnum((last_char = getchar())))
      identifier_str += last_char;

    if (identifier_str == "def") {
      return Token::k_tok_def;
    }
    if (identifier_str == "extern") {
      return Token::k_tok_extern;
    }
    if (identifier_str == "if") {
      return Token::k_tok_if;
    }
    if (identifier_str == "then") {
      return Token::k_tok_then;
    }
    if (identifier_str == "else") {
      return Token::k_tok_else;
    }
    if (identifier_str == "for") {
      return Token::k_tok_for;
    }
    if (identifier_str == "in") {
      return Token::k_tok_in;
    }
    if (identifier_str == "binary") {
      return Token::k_tok_binary;
    }
    if (identifier_str == "unary") {
      return Token::k_tok_unary;
    }
    return Token::k_tok_identifier;
  }
  if (isdigit(last_char) || last_char == '.') {
    // Number: [0-9.]+
    std::string NumStr;
    do {
      NumStr += last_char;
      last_char = getchar();
    } while (isdigit(last_char) || last_char == '.');

    num_val = strtod(NumStr.c_str(), nullptr);
    return Token::k_tok_number;
  }

  if (last_char == '#') {
    // Comment until end of line.
    do {
      last_char = getchar();
    } while (last_char != EOF && last_char != '\n' && last_char != '\r');

    if (last_char != EOF) {
      return get_tok();
    }
  }

  // Check for end of file.  Don't eat the EOF.
  if (last_char == EOF)
    return Token::k_tok_eof;

  // Otherwise, just return the character as its ascii value.
  int ThisChar = last_char;
  last_char = getchar();
  return static_cast<Token>(ThisChar);
}



Token get_next_token() {
  return cur_tok = get_tok();
}

Token get_current_token() {
  return cur_tok;
}