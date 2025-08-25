#pragma once

#include <cstdint>

/// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
/// of these for known things.
enum class Token : int32_t {
  k_tok_eof = -1,

  // commands
  k_tok_def = -2,
  k_tok_extern = -3,

  // primary
  k_tok_identifier = -4,
  k_tok_number = -5,

  // error handling
  k_tok_error = -6,
  // operators
  k_tok_l_paren = '(',
  k_tok_r_paren = ')',
  //control
  k_tok_if = -7,
  k_tok_then = -9,
  k_tok_else = -8,
  k_tok_for = -10,
  k_tok_in = -11,

  k_tok_binary = -12,
  k_tok_unary = -13
};
