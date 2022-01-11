#ifndef TORPUL_PRETTY_PRINTER_H
#define TORPUL_PRETTY_PRINTER_H

#include <iostream>
#include <sstream>

#include "parser.h"

namespace torpul {

namespace {

template <class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };
template <class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

std::string pretty_print_type(const TypeAST& ast) {
  return std::visit(overloaded{
                        [](const IoType& t) {
                          return std::string("IO<") + pretty_print_type(*t.base_type) + std::string(">");
                        },
                        [](const BuiltInNumberType& t) {
                          switch (t) {
                            case BuiltInNumberType::Integer:
                              return std::string("Integer");
                            case BuiltInNumberType::Float64:
                              return std::string("Float64");
                            case BuiltInNumberType::Float256:
                              return std::string("Float256");
                          }
                        },
                        [](const TruthValueType&) {
                          return std::string("TruthValue");
                        },
                        [](const UserDefinedType& t) {
                          return std::string("UserDefinedType(" + t.type_name + ")");
                        },
                        [](const std::monostate&) {
                          return std::string("ERROR_UNSPECIFIED_TYPE");
                        },
                    },
                    ast);
}

std::string number_type_suffix(const BuiltInNumberType& t) {
  switch (t) {
    case BuiltInNumberType::Integer:
      return "";
    case BuiltInNumberType::Float64:
      return "f64";
    case BuiltInNumberType::Float256:
      return "f256";
  }
}

std::string pretty_print_expression(const std::unique_ptr<ExpressionAST>& ast) {
  return std::visit(overloaded{
                        [](const ExpressionNumberAST& expr) {
                          return expr.number_string + number_type_suffix(expr.number_type);
                        },
                        [](const ExpressionTruthValueAST& expr) {
                          return expr.truth_value ? std::string("true") : std::string("false");
                        },
                        [](const ExpressionVariableAST& expr) {
                          return expr.variable_name;
                        },
                        [](const ExpressionFunctionCallAST& expr) {
                          std::stringstream ss;
                          ss << "call " << expr.function_name << "(";
                          bool first_argument = true;
                          for (const auto& [name, expr] : expr.arguments) {
                            if (first_argument) {
                              first_argument = false;
                            } else {
                              ss << ", ";
                            }
                            ss << name << "=" << pretty_print_expression(expr);
                          }
                          ss << ")";
                          return ss.str();
                        },
                        [](const ExpressionIfThenElseAST& expr) {
                          std::stringstream ss;
                          ss << "if " << pretty_print_expression(expr.condition) << " then " << pretty_print_expression(expr.then_clause) << " else " << pretty_print_expression(expr.else_clause) << " endif";
                          return ss.str();
                        },
                    },
                    *ast.get());
}

void pretty_print_function_body_statement(const std::unique_ptr<FunctionBodyStatementAST>& ast) {
  std::visit(overloaded{
                 [](const ReturnStatementAST& ret) {
                   std::cout << "  return " << pretty_print_expression(ret.return_value) << std::endl;
                 },
                 [](const DefineStatementAST& def) {
                   std::cout << "  define " << def.name << " as " << pretty_print_expression(def.value) << std::endl;
                 },
             },
             *ast.get());
}

void pretty_print_procedure_body_statement(const std::unique_ptr<ProcedureBodyStatementAST>& ast) {
  std::visit(overloaded{
                 [](const ReturnStatementAST& ret) {
                   std::cout << "  return " << pretty_print_expression(ret.return_value) << std::endl;
                 },
                 [](const DefineStatementAST& def) {
                   std::cout << "  define " << def.name << " as " << pretty_print_expression(def.value) << std::endl;
                 },
             },
             *ast.get());
}

std::string pretty_print_function_declaration_header(const FunctionDeclarationAST& decl) {
  std::stringstream ss;
  ss << decl.function_name << "(";
  bool first_parameter = true;
  for (const auto& [name, type] : decl.parameters) {
    if (first_parameter) {
      first_parameter = false;
    } else {
      ss << ", ";
    }
    ss << name << ": " << pretty_print_type(type);
  }
  ss << "): " << pretty_print_type(decl.function_return_type);
  return ss.str();
}

std::string pretty_print_procedure_declaration_header(const ProcedureDeclarationAST& decl) {
  std::stringstream ss;
  ss << decl.procedure_name << "(";
  bool first_parameter = true;
  for (const auto& [name, type] : decl.parameters) {
    if (first_parameter) {
      first_parameter = false;
    } else {
      ss << ", ";
    }
    ss << name << ": " << pretty_print_type(type);
  }
  ss << "): " << pretty_print_type(decl.procedure_return_type);
  return ss.str();
}
std::string pretty_print_extern_declaration_header(const ExternDeclarationAST& decl) {
  std::stringstream ss;
  ss << decl.function_name << "(";
  bool first_parameter = true;
  for (const auto& [name, type] : decl.parameters) {
    if (first_parameter) {
      first_parameter = false;
    } else {
      ss << ", ";
    }
    ss << name << ": " << pretty_print_type(type);
  }
  ss << "): " << pretty_print_type(decl.function_return_type);
  return ss.str();
}

void pretty_print_top_level_statement(const std::unique_ptr<TopLevelStatementAST>& ast) {
  std::visit(overloaded{
                 [](const FunctionDeclarationAST& decl) {
                   std::cout << "function " << pretty_print_function_declaration_header(decl) << std::endl;
                   for (const auto& statement : decl.statements) {
                     pretty_print_function_body_statement(statement);
                   }
                   std::cout << "endfunction" << std::endl;
                 },
                 [](const ProcedureDeclarationAST& decl) {
                   std::cout << "procedure " << pretty_print_procedure_declaration_header(decl) << std::endl;
                   for (const auto& statement : decl.statements) {
                     pretty_print_procedure_body_statement(statement);
                   }
                   std::cout << "endprocedure" << std::endl;
                 },
                 [](const ExternDeclarationAST& decl) {
                   std::cout << "extern function " << pretty_print_extern_declaration_header(decl) << std::endl;
                 },
             },
             *ast.get());
}

}  // namespace

void pretty_print(const std::unique_ptr<ProgramAST>& program) {
  for (const auto& statement : program->statements) {
    pretty_print_top_level_statement(statement);
    std::cout << std::endl;
  }
}

}  // namespace torpul

#endif
