#ifndef TORPUL_PARSER_H
#define TORPUL_PARSER_H

#define BOOST_STACKTRACE_GNU_SOURCE_NOT_REQUIRED 1

#include <boost/stacktrace.hpp>
#include <cassert>
#include <iostream>
#include <map>
#include <sstream>
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

bool operator==(const UserDefinedType& a, const UserDefinedType& b) {
  return a.type_name == b.type_name;
}

struct TruthValueType {
};

bool operator==(const TruthValueType& a, const TruthValueType& b) {
  return true;
}

struct IoType;

// TODO: remove monostate; it's not supported, just there to catch default-initialization errors
using TypeAST = std::variant<std::monostate, BuiltInNumberType, TruthValueType, UserDefinedType, IoType>;

struct IoType {
  std::shared_ptr<TypeAST> base_type;
};

bool operator==(const IoType& a, const IoType& b) {
  return *a.base_type == *b.base_type;
}

bool is_iotype(const TypeAST& ast) {
  const auto* maybe_io = std::get_if<IoType>(&ast);
  return maybe_io != nullptr;
}

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
struct ExpressionProcedureCallAST;
struct ExpressionIfThenElseAST;

using ExpressionAST = std::variant<ExpressionNumberAST, ExpressionTruthValueAST, ExpressionVariableAST, ExpressionFunctionCallAST, ExpressionProcedureCallAST, ExpressionIfThenElseAST>;

struct ExpressionFunctionCallAST {
  std::string function_name;
  std::map<std::string, std::unique_ptr<ExpressionAST>> arguments;
};

struct ExpressionProcedureCallAST {
  std::string procedure_name;
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

struct DefineStatementAST {
  std::string name;
  std::unique_ptr<ExpressionAST> value;
};

struct RunStatementAST {
  std::vector<std::unique_ptr<ExpressionAST>> expressions;
};

using FunctionBodyStatementAST = std::variant<ReturnStatementAST, DefineStatementAST>;
using ProcedureBodyStatementAST = std::variant<ReturnStatementAST, DefineStatementAST, RunStatementAST>;

struct FunctionDeclarationAST {
  std::string function_name;
  TypeAST function_return_type;
  std::map<std::string, TypeAST> parameters;
  std::vector<std::unique_ptr<FunctionBodyStatementAST>> statements;
};

struct ProcedureDeclarationAST {
  std::string procedure_name;
  TypeAST procedure_return_type;
  std::map<std::string, TypeAST> parameters;
  std::vector<std::unique_ptr<ProcedureBodyStatementAST>> statements;
};

struct ExternFunctionDeclarationAST {
  std::string function_name;
  TypeAST function_return_type;
  std::map<std::string, TypeAST> parameters;
};

struct ExternProcedureDeclarationAST {
  std::string procedure_name;
  TypeAST procedure_return_type;
  std::map<std::string, TypeAST> parameters;
};

using TopLevelStatementAST = std::variant<FunctionDeclarationAST, ProcedureDeclarationAST, ExternProcedureDeclarationAST, ExternFunctionDeclarationAST>;

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
        fail_unexpected("Repeated parameter name: ");
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

  ExpressionProcedureCallAST ParseProcedureCallExpression() {
    consume(Token::term_callproc);
    const std::string name = consume_identifier();
    consume(Token::open_paren);
    std::map<std::string, std::unique_ptr<ExpressionAST>> parameters;
    while (last_token != Token::close_paren) {
      const std::string param_name = consume_identifier();
      if (parameters.count(param_name)) {
        fail_unexpected("Repeated parameter name: ");
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
      std::cerr << "parsed ExpressionProcedureCallAST: " << name << std::endl;
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
      case Token::term_callproc:
        return std::make_unique<ExpressionAST>(ExpressionAST({ParseProcedureCallExpression()}));
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
        fail_unexpected("Expected expression but found:");
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

  DefineStatementAST ParseDefineStatement() {
    consume(Token::term_define);
    const std::string name = consume_identifier();
    consume(Token::term_as);
    std::unique_ptr<ExpressionAST> expression = ParseExpression();
    if (mode == Mode::Verbose) {
      std::cerr << "parsed DefineStatementAST" << std::endl;
    }
    return {name, std::move(expression)};
  }

  RunStatementAST ParseRunStatement() {
    consume(Token::term_run);
    std::vector<std::unique_ptr<ExpressionAST>> expressions;
    bool done = false;
    while (!done) {
      switch (last_token) {
        case Token::term_do: {
          consume(Token::term_do);
          std::unique_ptr<ExpressionAST> expression = ParseExpression();
          expressions.push_back(std::move(expression));
          break;
        }
        case Token::term_endrun:
          done = true;
          break;
        default:
          fail_unexpected("Expected run IO statement but found: ");
          break;
      }
    }
    consume(Token::term_endrun);
    if (mode == Mode::Verbose) {
      std::cerr << "parsed RunStatementAST" << std::endl;
    }
    return {std::move(expressions)};
  }

  std::unique_ptr<FunctionBodyStatementAST> ParseFunctionBodyStatement() {
    switch (last_token) {
      case Token::term_return:
        return make_unique<FunctionBodyStatementAST>(FunctionBodyStatementAST({ParseReturnStatement()}));
      case Token::term_define:
        return make_unique<FunctionBodyStatementAST>(FunctionBodyStatementAST({ParseDefineStatement()}));
      default:
        fail_unexpected("Expected function body statement but found: ");
    }
  }

  std::unique_ptr<ProcedureBodyStatementAST> ParseProcedureBodyStatement() {
    switch (last_token) {
      case Token::term_return:
        return make_unique<ProcedureBodyStatementAST>(ProcedureBodyStatementAST({ParseReturnStatement()}));
      case Token::term_define:
        return make_unique<ProcedureBodyStatementAST>(ProcedureBodyStatementAST({ParseDefineStatement()}));
      case Token::term_run:
        return make_unique<ProcedureBodyStatementAST>(ProcedureBodyStatementAST({ParseRunStatement()}));
      default:
        fail_unexpected("Expected procedure body statement but found: ");
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
        fail_unexpected("Repeated parameter name: ");
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

  ProcedureDeclarationAST ParseProcedureDeclaration() {
    consume(Token::term_procedure);
    const std::string procedure_name = consume_identifier();
    consume(Token::open_paren);
    std::map<std::string, TypeAST> parameters;
    while (last_token == Token::identifier_string) {
      const std::string param_name = consume_identifier();
      consume(Token::colon);
      const TypeAST param_type = consume_type();
      if (parameters.count(param_name)) {
        fail_unexpected("Repeated parameter name: ");
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
    const TypeAST procedure_return_type = consume_type();
    std::vector<std::unique_ptr<ProcedureBodyStatementAST>> statements;
    while (last_token != Token::term_endprocedure) {
      statements.push_back(ParseProcedureBodyStatement());
    }
    consume(Token::term_endprocedure);
    if (mode == Mode::Verbose) {
      std::cerr << "parsed ProcedureDeclarationAST: " << procedure_name << std::endl;
    }
    return {
        procedure_name,
        procedure_return_type,
        parameters,
        std::move(statements),
    };
  }

  std::variant<ExternFunctionDeclarationAST, ExternProcedureDeclarationAST> ParseExternDeclaration() {
    bool is_procedure = false;
    consume(Token::term_extern);
    if (last_token == Token::term_procedure) {
      is_procedure = true;
      consume(Token::term_procedure);
    } else {
      consume(Token::term_function);
    }
    const std::string extern_name = consume_identifier();
    consume(Token::open_paren);
    std::map<std::string, TypeAST> parameters;
    while (last_token == Token::identifier_string) {
      const std::string param_name = consume_identifier();
      consume(Token::colon);
      const TypeAST param_type = consume_type();
      if (parameters.count(param_name)) {
        fail_unexpected("Repeated parameter name: ");
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
    const TypeAST return_type = consume_type();
    if (is_procedure) {
      if (mode == Mode::Verbose) {
        std::cerr << "parsed ExternProcedureDeclarationAST: " << extern_name << std::endl;
      }
      return ExternProcedureDeclarationAST({
          extern_name,
          return_type,
          parameters,
      });
    } else {
      if (mode == Mode::Verbose) {
        std::cerr << "parsed ExternFunctionDeclarationAST: " << extern_name << std::endl;
      }
      return ExternFunctionDeclarationAST({
          extern_name,
          return_type,
          parameters,
      });
    }
  }
  std::unique_ptr<TopLevelStatementAST> ParseTopLevelStatement() {
    switch (last_token) {
      case Token::term_function:
        return std::make_unique<TopLevelStatementAST>(TopLevelStatementAST({ParseFunctionDeclaration()}));
      case Token::term_procedure:
        return std::make_unique<TopLevelStatementAST>(TopLevelStatementAST({ParseProcedureDeclaration()}));
      case Token::term_extern:
        return std::visit(overloaded{
                              [&](const ExternFunctionDeclarationAST& func) {
                                return std::make_unique<TopLevelStatementAST>(func);
                              },
                              [&](const ExternProcedureDeclarationAST& proc) {
                                return std::make_unique<TopLevelStatementAST>(proc);
                              },
                          },
                          ParseExternDeclaration());
      default:
        fail_unexpected("Expected top-level statement but found: ");
    }
  }

  std::string consume_identifier() {
    if (last_token != Token::identifier_string) {
      fail_unexpected("Expected identified but found: ");
    }
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
    } else if (name == "IO") {
      consume(Token::symbol_lessthan);
      auto base_type = std::make_shared<TypeAST>(consume_type());
      consume(Token::symbol_greaterthan);
      return IoType{base_type};
    } else {
      fail_unexpected("Expected type but found: ");
    }
  }

  void consume(Token t) {
    if (last_token != t) {
      std::stringstream ss;
      ss << t;
      fail_unexpected("Expected " + ss.str() + " but found: ");
    }
    read_next_token();
  }

  void read_next_token() {
    auto value = lexer.read_token();
    last_token = value.first;
    last_token_line_number = value.second;
  }

  Parser(Lexer* lexer, Mode mode) : lexer(*lexer), last_token(Token::eof), mode(mode) {}
  Lexer& lexer;
  Token last_token;
  int last_token_line_number;
  const Mode mode;

  [[noreturn]] void fail_unexpected(std::string message) {
    std::cerr << message << last_token << std::endl;
    std::cerr << "  on line " << last_token_line_number << ": " << lexer.get_consumed_line_content_before_last_token() << " ... " << lexer.consume_rest_of_line_for_error_message() << std::endl;
    std::cerr << boost::stacktrace::stacktrace();
    assert(false);
  }
};

}  // namespace torpul

#endif
