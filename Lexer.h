#pragma once

#include <string>

#include "Token.h"

// Global lexer state
extern std::string identifier_str;  // Filled in if tok_identifier
extern double num_val;              // Filled in if tok_number
extern int line_num;                // Track line numbers for error reporting

/// gettok - Return the next token from standard input.
Token GetTok();

/// GetNextToken - Read another token from the lexer and update CurTok.
Token GetNextToken();

/// Get current token
Token GetCurrentToken();
