#ifndef TORPUL_PARSER_H
#define TORPUL_PARSER_H

#include <cassert>
#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lexer.h"

namespace torpul {

enum class BuiltInNumberType {
  Integer,
  Float64,
  Float256,
};

struct UserDefinedType {
  std::string type_name;
};

struct TruthValueType {
};

bool operator==(const UserDefinedType& a, const UserDefinedType& b) {
  return a.type_name == b.type_name;
}

bool operator==(const TruthValueType& a, const TruthValueType& b) {
  return true;
}

// TODO: remove monostate; it's not supported, just there to catch default-initialization errors
using TypeAST = std::variant<std::monostate, BuiltInNumberType, TruthValueType, UserDefinedType>;

struct ExpressionNumberAST {
  std::string number_string;
  BuiltInNumberType number_type;
};

struct ExpressionTruthValueAST {
  bool truth_value;
};

struct ExpressionVariableAST {
  std::string variable_name;
};

struct ExpressionFunctionCallAST;
struct ExpressionIfThenElseAST;

using ExpressionAST = std::variant<ExpressionNumberAST, ExpressionTruthValueAST, ExpressionVariableAST, ExpressionFunctionCallAST, ExpressionIfThenElseAST>;

struct ExpressionFunctionCallAST {
  std::string function_name;
  std::map<std::string, std::unique_ptr<ExpressionAST>> arguments;
};

struct ExpressionIfThenElseAST {
  std::unique_ptr<ExpressionAST> condition;
  std::unique_ptr<ExpressionAST> then_clause;
  std::unique_ptr<ExpressionAST> else_clause;
};

struct ReturnStatementAST {
  std::unique_ptr<ExpressionAST> return_value;
};

using FunctionBodyStatementAST = std::variant<ReturnStatementAST>;

struct FunctionDeclarationAST {
  std::string function_name;
  TypeAST function_return_type;
  std::map<std::string, TypeAST> parameters;
  std::vector<std::unique_ptr<FunctionBodyStatementAST>> statements;
};

struct ExternDeclarationAST {
  std::string function_name;
  TypeAST function_return_type;
  std::map<std::string, TypeAST> parameters;
};

using TopLevelStatementAST = std::variant<FunctionDeclarationAST, ExternDeclarationAST>;

struct ProgramAST {
  std::vector<std::unique_ptr<TopLevelStatementAST>> statements;
};

class Parser {
 public:
  enum class Mode {
    Quiet,
    Verbose,
  };

  static Parser Create(Lexer* lexer, Mode mode = Mode::Quiet) {
    return Parser(lexer, mode);
  }

  std::unique_ptr<ProgramAST> ParseProgram() {
    read_next_token();
    std::vector<std::unique_ptr<TopLevelStatementAST>> statements;
    while (last_token != Token::eof) {
      statements.push_back(ParseTopLevelStatement());
    }
    return std::make_unique<ProgramAST>(ProgramAST({std::move(statements)}));
  }

 private:
  ExpressionVariableAST ParseVariableExpression() {
    const std::string name = consume_identifier();
    if (mode == Mode::Verbose) {
      std::cerr << "parsed ExpressionVariableAST: " << name << std::endl;
    }
    return {name};
  }

  ExpressionFunctionCallAST ParseFunctionCallExpression() {
    consume(Token::term_call);
    const std::string name = consume_identifier();
    consume(Token::open_paren);
    std::map<std::string, std::unique_ptr<ExpressionAST>> parameters;
    while (last_token != Token::close_paren) {
      const std::string param_name = consume_identifier();
      if (parameters.count(param_name)) {
        std::cerr << "Error: Repeated parameter name: " << param_name << std::endl;
        assert(false);
      }
      consume(Token::equals);
      std::unique_ptr<ExpressionAST> param_value = ParseExpression();
      parameters.insert(std::make_pair(param_name, std::move(param_value)));
      if (last_token == Token::comma) {
        consume(Token::comma);
        continue;
      } else {
        break;
      }
    }
    consume(Token::close_paren);
    if (mode == Mode::Verbose) {
      std::cerr << "parsed ExpressionFunctionCallAST: " << name << std::endl;
    }
    return {name, std::move(parameters)};
  }

  ExpressionNumberAST ParseNumberExpression() {
    switch (last_token) {
      case Token::const_integer_string: {
        const std::string number = consume_number();
        if (mode == Mode::Verbose) {
          std::cerr << "parsed ExpressionNumberAST integer: " << number << std::endl;
        }
        return {number, BuiltInNumberType::Integer};
      }
      case Token::const_float64_string: {
        const std::string number = consume_number();
        if (mode == Mode::Verbose) {
          std::cerr << "parsed ExpressionNumberAST float64: " << number << std::endl;
        }
        return {number, BuiltInNumberType::Float64};
      }
      case Token::const_float256_string: {
        const std::string number = consume_number();
        if (mode == Mode::Verbose) {
          std::cerr << "parsed ExpressionNumberAST float256: " << number << std::endl;
        }
        return {number, BuiltInNumberType::Float256};
      }
      default:
        std::cerr << "Internal error: ParseNumberExpression() called when next token was: " << last_token << std::endl;
        assert(false);
    }
  }

  ExpressionIfThenElseAST ParseIfThenElseExpression() {
    consume(Token::term_if);
    std::unique_ptr<ExpressionAST> condition = ParseExpression();
    consume(Token::term_then);
    std::unique_ptr<ExpressionAST> then_clause = ParseExpression();
    consume(Token::term_else);
    std::unique_ptr<ExpressionAST> else_clause = ParseExpression();
    consume(Token::term_endif);
    if (mode == Mode::Verbose) {
      std::cerr << "parsed ExpressionIfThenElseAST" << std::endl;
    }
    return {std::move(condition), std::move(then_clause), std::move(else_clause)};
  }

  std::unique_ptr<ExpressionAST> ParseExpression() {
    switch (last_token) {
      case Token::identifier_string:
        return std::make_unique<ExpressionAST>(ExpressionAST({ParseVariableExpression()}));
      case Token::term_call:
        return std::make_unique<ExpressionAST>(ExpressionAST({ParseFunctionCallExpression()}));
      case Token::const_integer_string:
      case Token::const_float64_string:
      case Token::const_float256_string:
        return std::make_unique<ExpressionAST>(ExpressionAST({ParseNumberExpression()}));
      case Token::const_true:
        consume(Token::const_true);
        return std::make_unique<ExpressionAST>(ExpressionTruthValueAST({true}));
      case Token::const_false:
        consume(Token::const_false);
        return std::make_unique<ExpressionAST>(ExpressionTruthValueAST({false}));
      case Token::term_if:
        return std::make_unique<ExpressionAST>(ExpressionAST({ParseIfThenElseExpression()}));
      default:
        std::cerr << "Error: Expected expression but found: " << last_token << std::endl;
        assert(false);
    }
  }

  ReturnStatementAST ParseReturnStatement() {
    consume(Token::term_return);
    std::unique_ptr<ExpressionAST> expression = ParseExpression();
    if (mode == Mode::Verbose) {
      std::cerr << "parsed ReturnStatementAST" << std::endl;
    }
    return {std::move(expression)};
  }

  std::unique_ptr<FunctionBodyStatementAST> ParseFunctionBodyStatement() {
    switch (last_token) {
      case Token::term_return:
        return make_unique<FunctionBodyStatementAST>(FunctionBodyStatementAST({ParseReturnStatement()}));
      default:
        std::cerr << "Error: Expected function body statement but found: " << last_token << std::endl;
        assert(false);
    }
  }

  FunctionDeclarationAST ParseFunctionDeclaration() {
    consume(Token::term_function);
    const std::string function_name = consume_identifier();
    consume(Token::open_paren);
    std::map<std::string, TypeAST> parameters;
    while (last_token == Token::identifier_string) {
      const std::string param_name = consume_identifier();
      consume(Token::colon);
      const TypeAST param_type = consume_type();
      if (parameters.count(param_name)) {
        std::cerr << "Error: Repeated parameter name: " << param_name << std::endl;
        assert(false);
      }
      parameters[param_name] = param_type;
      if (last_token == Token::comma) {
        consume(Token::comma);
        continue;
      } else {
        break;
      }
    }
    consume(Token::close_paren);
    consume(Token::colon);
    const TypeAST function_return_type = consume_type();
    std::vector<std::unique_ptr<FunctionBodyStatementAST>> statements;
    while (last_token != Token::term_endfunction) {
      statements.push_back(ParseFunctionBodyStatement());
    }
    consume(Token::term_endfunction);
    if (mode == Mode::Verbose) {
      std::cerr << "parsed FunctionDeclarationAST: " << function_name << std::endl;
    }
    return {
        function_name,
        function_return_type,
        parameters,
        std::move(statements),
    };
  }

  ExternDeclarationAST ParseExternDeclaration() {
    consume(Token::term_extern);
    consume(Token::term_function);
    const std::string function_name = consume_identifier();
    consume(Token::open_paren);
    std::map<std::string, TypeAST> parameters;
    while (last_token == Token::identifier_string) {
      const std::string param_name = consume_identifier();
      consume(Token::colon);
      const TypeAST param_type = consume_type();
      if (parameters.count(param_name)) {
        std::cerr << "Error: Repeated parameter name: " << param_name << std::endl;
        assert(false);
      }
      parameters[param_name] = param_type;
      if (last_token == Token::comma) {
        consume(Token::comma);
        continue;
      } else {
        break;
      }
    }
    consume(Token::close_paren);
    consume(Token::colon);
    const TypeAST function_return_type = consume_type();
    if (mode == Mode::Verbose) {
      std::cerr << "parsed ExternDeclarationAST: " << function_name << std::endl;
    }
    return {
        function_name,
        function_return_type,
        parameters,
    };
  }
  std::unique_ptr<TopLevelStatementAST> ParseTopLevelStatement() {
    switch (last_token) {
      case Token::term_function:
        return make_unique<TopLevelStatementAST>(TopLevelStatementAST({ParseFunctionDeclaration()}));
      case Token::term_extern:
        return make_unique<TopLevelStatementAST>(TopLevelStatementAST({ParseExternDeclaration()}));
      default:
        std::cerr << "Error: Expected top-level statement but found: " << last_token << std::endl;
        assert(false);
    }
  }

  std::string consume_identifier() {
    std::string name = lexer.get_identifier_content();
    consume(Token::identifier_string);
    return name;
  }

  std::string consume_number() {
    std::string content = lexer.get_number_content();
    switch (last_token) {
      case Token::const_integer_string:
      case Token::const_float64_string:
      case Token::const_float256_string:
        consume(last_token);
        return content;
      default:
        assert(false);
    }
  }

  TypeAST consume_type() {
    std::string name = lexer.get_identifier_content();
    consume(Token::identifier_string);
    if (name == "Integer") {
      return BuiltInNumberType::Integer;
    } else if (name == "Float64") {
      return BuiltInNumberType::Float64;
    } else if (name == "Float256") {
      return BuiltInNumberType::Float256;
    } else if (name == "TruthValue") {
      return TruthValueType{};
    } else {
      std::cerr << "Error: Expected type but found: " << name << std::endl;
      assert(false);
    }
  }

  void consume(Token t) {
    if (last_token != t) {
      std::cerr << "Error: Expected " << t << " but found: " << last_token << std::endl;
      assert(false);
    }
    read_next_token();
  }

  void read_next_token() {
    last_token = lexer.read_token();
  }

  Parser(Lexer* lexer, Mode mode) : lexer(*lexer), last_token(Token::eof), mode(mode) {}
  Lexer& lexer;
  Token last_token;
  const Mode mode;
};

}  // namespace torpul

#endif
