#ifndef TORPUL_PRETTY_PRINTER_H
#define TORPUL_PRETTY_PRINTER_H

#include <iostream>
#include <sstream>

#include "parser.h"

namespace torpul {

namespace {

std::string pretty_print_parameter_list(const ParameterList& parameters);

std::string pretty_print_type(const TypeAST& ast) {
  if (ast.valueless_by_exception()) {
    std::cerr << "Oh no! TypeAST is valueless by exception. That shouldn't happen!" << std::endl;
  }
  return std::visit(overloaded{
                        [](const FunctionType& t) {
                          return std::string("Function<(" + pretty_print_parameter_list(t.parameters) + "):" + pretty_print_type(*t.return_type) + ">");
                        },
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
                        [](const ExpressionProcedureCallAST& expr) {
                          std::stringstream ss;
                          ss << "callproc " << expr.procedure_name << "(";
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

std::string pretty_print_parameter_list(const ParameterList& parameters) {
  std::stringstream ss;
  bool first_parameter = true;
  for (const auto& [name, type] : parameters) {
    if (first_parameter) {
      first_parameter = false;
    } else {
      ss << ", ";
    }
    ss << name << ": " << pretty_print_type(type);
  }
  return ss.str();
}

std::string pretty_print_function_declaration_header(const FunctionDeclarationAST& decl) {
  return decl.function_name + "(" + pretty_print_parameter_list(decl.parameters) + "): " + pretty_print_type(decl.function_return_type);
}

std::string pretty_print_extern_function_declaration_header(const ExternFunctionDeclarationAST& decl) {
  return decl.function_name + "(" + pretty_print_parameter_list(decl.parameters) + "): " + pretty_print_type(decl.function_return_type);
}

std::string pretty_print_procedure_declaration_header(const ProcedureDeclarationAST& decl) {
  return decl.procedure_name + "(" + pretty_print_parameter_list(decl.parameters) + "): " + pretty_print_type(decl.procedure_return_type);
}

std::string pretty_print_extern_procedure_declaration_header(const ExternProcedureDeclarationAST& decl) {
  return decl.procedure_name + "(" + pretty_print_parameter_list(decl.parameters) + "): " + pretty_print_type(decl.procedure_return_type);
}

void pretty_print_function_body_statement(const std::unique_ptr<FunctionBodyStatementAST>& ast) {
  std::visit(overloaded{
                 [](const ReturnStatementAST& ret) {
                   std::cout << "  return " << pretty_print_expression(ret.return_value) << std::endl;
                 },
                 [](const DefineStatementAST& def) {
                   std::cout << "  define " << def.name << " as " << pretty_print_expression(def.value) << std::endl;
                 },
                 [](const FunctionDeclarationAST& decl) {
                   std::cout << "  function " << pretty_print_function_declaration_header(decl) << std::endl;
                   for (const auto& statement : decl.statements) {
                     std::cout << "  ";
                     pretty_print_function_body_statement(statement);
                   }
                   std::cout << "  endfunction" << std::endl;
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
                 [](const RunStatementAST& run) {
                   std::cout << "  run" << std::endl;
                   for (const auto& e : run.expressions) {
                     std::cout << "    do " << pretty_print_expression(e) << std::endl;
                   }
                   std::cout << "  endrun" << std::endl;
                 },
             },
             *ast.get());
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
                 [](const DefineStatementAST& def) {
                   std::cout << "define " << def.name << " as " << pretty_print_expression(def.value) << std::endl;
                 },
                 [](const ProcedureDeclarationAST& decl) {
                   std::cout << "procedure " << pretty_print_procedure_declaration_header(decl) << std::endl;
                   for (const auto& statement : decl.statements) {
                     pretty_print_procedure_body_statement(statement);
                   }
                   std::cout << "endprocedure" << std::endl;
                 },
                 [](const ExternFunctionDeclarationAST& decl) {
                   std::cout << "extern function " << pretty_print_extern_function_declaration_header(decl) << std::endl;
                 },
                 [](const ExternProcedureDeclarationAST& decl) {
                   std::cout << "extern procedure " << pretty_print_extern_procedure_declaration_header(decl) << std::endl;
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
