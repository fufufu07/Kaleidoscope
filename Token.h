#pragma once

#include <cstdint>

/// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
/// of these for known things.
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
  // operators
  kTokLParen = '(',
  kTokRParen = ')',
};
