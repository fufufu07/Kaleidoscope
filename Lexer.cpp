#include "Lexer.h"

#include <cctype>
#include <cstdlib>
#include <iostream>
#include <print>
#include <unordered_map>

// Global lexer state
std::string identifier_str;  // Filled in if tok_identifier
double num_val;              // Filled in if tok_number
int line_num = 1;            // Track line numbers for error reporting

// Cache for keywords to avoid string comparisons
static const std::unordered_map<std::string, Token> kKeywords = {
    {"def", Token::kTokDef}, {"extern", Token::kTokExtern}};

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
    last_char = std::cin.get();
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
    last_char = std::cin.get();
  } while (last_char != EOF && last_char != '\n' && last_char != '\r');

  if (last_char == '\n') {
    ++line_num;
  }

  return last_char != EOF;
}

Token get_tok() {
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
      return get_tok();
    }
  }

  if (last_char == EOF) {
    return Token::kTokEof;
  }

  const int this_char = last_char;
  last_char = std::cin.get();
  return static_cast<Token>(this_char);
}

Token get_next_token() {
  return cur_tok = get_tok();
}

Token get_current_token() {
  return cur_tok;
}
