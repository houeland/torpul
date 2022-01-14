#ifndef TORPUL_LEXER_H
#define TORPUL_LEXER_H

#include <cassert>
#include <iostream>
#include <string>

namespace torpul {

namespace {

template <class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };
template <class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

}  // namespace

enum class Token {
  eof,
  term_function,
  term_endfunction,
  term_return,
  term_call,
  term_callproc,
  term_if,
  term_then,
  term_else,
  term_endif,
  term_extern,
  term_define,
  term_as,
  term_procedure,
  term_endprocedure,
  term_run,
  term_endrun,
  term_do,
  identifier_string,
  const_integer_string,
  const_float64_string,
  const_float256_string,
  const_true,
  const_false,
  open_paren,
  close_paren,
  symbol_lessthan,
  symbol_greaterthan,
  colon,
  equals,
  comma,
  error_unknown_token,
  error_invalid_number_format,
};

std::ostream& operator<<(std::ostream& os, const Token& t) {
  switch (t) {
    case Token::eof:
      os << "eof";
      return os;
    case Token::term_function:
      os << "term_function";
      return os;
    case Token::term_endfunction:
      os << "term_endfunction";
      return os;
    case Token::term_return:
      os << "term_return";
      return os;
    case Token::term_call:
      os << "term_call";
      return os;
    case Token::term_callproc:
      os << "term_callproc";
      return os;
    case Token::term_if:
      os << "term_if";
      return os;
    case Token::term_then:
      os << "term_then";
      return os;
    case Token::term_else:
      os << "term_else";
      return os;
    case Token::term_endif:
      os << "term_endif";
      return os;
    case Token::term_extern:
      os << "term_extern";
      return os;
    case Token::term_define:
      os << "term_define";
      return os;
    case Token::term_as:
      os << "term_as";
      return os;
    case Token::term_procedure:
      os << "term_procedure";
      return os;
    case Token::term_endprocedure:
      os << "term_endprocedure";
      return os;
    case Token::term_run:
      os << "term_run";
      return os;
    case Token::term_endrun:
      os << "term_endrun";
      return os;
    case Token::term_do:
      os << "term_do";
      return os;
    case Token::identifier_string:
      os << "identifier_string";
      return os;
    case Token::const_integer_string:
      os << "const_integer_string";
      return os;
    case Token::const_float64_string:
      os << "const_float64_string";
      return os;
    case Token::const_float256_string:
      os << "const_float256_string";
      return os;
    case Token::const_true:
      os << "const_true";
      return os;
    case Token::const_false:
      os << "const_false";
      return os;
    case Token::open_paren:
      os << "open_paren";
      return os;
    case Token::close_paren:
      os << "close_paren";
      return os;
    case Token::symbol_lessthan:
      os << "symbol_lessthan";
      return os;
    case Token::symbol_greaterthan:
      os << "symbol_greaterthan";
      return os;
    case Token::colon:
      os << "colon";
      return os;
    case Token::equals:
      os << "equals";
      return os;
    case Token::comma:
      os << "comma";
      return os;
    case Token::error_unknown_token:
      os << "error_unknown_token";
      return os;
    case Token::error_invalid_number_format:
      os << "error_invalid_number_format";
      return os;
  }
}

class Lexer {
 public:
  std::pair<Token, int> read_token() {
    previous_consumed_line_so_far = latest_consumed_line_so_far;
    Token token = read_token_impl();
    if (mode == Mode::Verbose) {
      show_read_token(token);
    }
    return std::make_pair(token, line_number);
  }

  enum class Mode {
    Quiet,
    Verbose,
  };

  static Lexer Create(Mode mode = Mode::Quiet) {
    return Lexer(mode);
  }

  std::string get_identifier_content() {
    assert(current_identifier_content != "");
    return current_identifier_content;
  }

  std::string get_number_content() {
    assert(current_number_content != "");
    return current_number_content;
  }

  std::string get_consumed_line_content_before_last_token() {
    return previous_consumed_line_so_far;
  }

  std::string consume_rest_of_line_for_error_message() {
    std::string rest;
    while (true) {
      char c = getchar();
      if (c == EOF || c == '\n') break;
      rest += c;
    }
    return rest;
  }

 private:
  Lexer(Mode mode) : mode(mode) {}
  std::string current_identifier_content;
  std::string current_number_content;
  int last_char = ' ';
  int line_number = 1;
  std::string latest_consumed_line_so_far;
  std::string previous_consumed_line_so_far;
  const Mode mode;

  int read_next_char() {
    int c = getchar();
    latest_consumed_line_so_far += c;
    if (c == '\n') {
      line_number += 1;
      latest_consumed_line_so_far = "";
    }
    return c;
  }

  Token read_token_impl() {
    current_identifier_content = "";
    current_number_content = "";
    while (isspace(last_char)) {
      last_char = read_next_char();
    }

    if (isalpha(last_char)) {
      std::string identifier;
      do {
        identifier += (char)last_char;
        last_char = read_next_char();
      } while (isalnum(last_char));
      //   std::cerr << "read identifier: " << identifier << std::endl;
      if (identifier == "function") {
        return Token::term_function;
      } else if (identifier == "endfunction") {
        return Token::term_endfunction;
      } else if (identifier == "return") {
        return Token::term_return;
      } else if (identifier == "call") {
        return Token::term_call;
      } else if (identifier == "callproc") {
        return Token::term_callproc;
      } else if (identifier == "if") {
        return Token::term_if;
      } else if (identifier == "then") {
        return Token::term_then;
      } else if (identifier == "else") {
        return Token::term_else;
      } else if (identifier == "endif") {
        return Token::term_endif;
      } else if (identifier == "extern") {
        return Token::term_extern;
      } else if (identifier == "define") {
        return Token::term_define;
      } else if (identifier == "as") {
        return Token::term_as;
      } else if (identifier == "procedure") {
        return Token::term_procedure;
      } else if (identifier == "endprocedure") {
        return Token::term_endprocedure;
      } else if (identifier == "run") {
        return Token::term_run;
      } else if (identifier == "endrun") {
        return Token::term_endrun;
      } else if (identifier == "do") {
        return Token::term_do;
      } else if (identifier == "true") {
        return Token::const_true;
      } else if (identifier == "false") {
        return Token::const_false;
      } else {
        current_identifier_content = identifier;
        return Token::identifier_string;
      }
    } else if (isdigit(last_char)) {
      std::string number = "";
      do {
        number += last_char;
        last_char = read_next_char();
      } while (isdigit(last_char));
      if (last_char == '.') {
        do {
          number += last_char;
          last_char = read_next_char();
        } while (isdigit(last_char));
        std::string suffix = "";
        do {
          suffix += last_char;
          last_char = read_next_char();
        } while (isalnum(last_char));
        if (suffix == "f64") {
          current_number_content = number;
          return Token::const_float64_string;
        } else if (suffix == "f256") {
          current_number_content = number;
          return Token::const_float256_string;
        } else {
          std::cerr << "Error: expected f64 or f256 float constant suffix, but got: " << suffix << std::endl;
          std::cerr << "Valid number constant formats are: 1234 (Integer), 12.34f64 (Float64), 12.34f256 (Float256)" << std::endl;
          return Token::error_invalid_number_format;
        }
      } else {
        current_number_content = number;
        return Token::const_integer_string;
      }
    } else if (last_char == '(') {
      last_char = ' ';
      return Token::open_paren;
    } else if (last_char == ')') {
      last_char = ' ';
      return Token::close_paren;
    } else if (last_char == '<') {
      last_char = ' ';
      return Token::symbol_lessthan;
    } else if (last_char == '>') {
      last_char = ' ';
      return Token::symbol_greaterthan;
    } else if (last_char == ':') {
      last_char = ' ';
      return Token::colon;
    } else if (last_char == '=') {
      last_char = ' ';
      return Token::equals;
    } else if (last_char == ',') {
      last_char = ' ';
      return Token::comma;
    } else if (last_char == EOF) {
      last_char = ' ';
      return Token::eof;
    } else {
      return Token::error_unknown_token;
    }
  }

  void show_read_token(Token token) {
    switch (token) {
      case Token::identifier_string:
        std::cerr << "read identifier: " << get_identifier_content() << std::endl;
        return;
      case Token::const_integer_string:
      case Token::const_float64_string:
      case Token::const_float256_string:
        std::cerr << "read number constant: " << get_number_content() << std::endl;
        return;
      default:
        std::cerr << "read token: " << token << std::endl;
        return;
    }
  }
};

}  // namespace torpul

#endif
