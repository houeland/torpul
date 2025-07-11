#ifndef TORPUL_TYPER_H
#define TORPUL_TYPER_H

#include <cassert>
#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "parser.h"
#include "pretty_printer.h"

namespace torpul {

struct TypedExpressionNumberAST {
  std::string number_string;
  TypeAST type;
};

struct TypedExpressionTruthValueAST {
  bool truth_value;
  TypeAST type;
};

struct TypedExpressionVariableAST {
  std::string variable_name;
  TypeAST type;
};

struct TypedExpressionFunctionCallAST;
struct TypedExpressionProcedureCallAST;
struct TypedExpressionIfThenElseAST;

struct TypedExpressionAST {
  std::variant<std::unique_ptr<TypedExpressionNumberAST>, std::unique_ptr<TypedExpressionTruthValueAST>, std::unique_ptr<TypedExpressionVariableAST>, std::unique_ptr<TypedExpressionProcedureCallAST>, std::unique_ptr<TypedExpressionFunctionCallAST>, std::unique_ptr<TypedExpressionIfThenElseAST>> value;
  TypeAST type;
};

struct TypedExpressionFunctionCallAST {
  std::string function_name;
  std::map<std::string, TypedExpressionAST> parameter_values;
  TypeAST type;
};

struct TypedExpressionProcedureCallAST {
  std::string procedure_name;
  std::map<std::string, TypedExpressionAST> parameter_values;
  TypeAST type;
};

struct TypedExpressionIfThenElseAST {
  TypedExpressionAST condition;
  TypedExpressionAST then_clause;
  TypedExpressionAST else_clause;
  TypeAST type;
};

struct TypedReturnStatementAST {
  TypedExpressionAST return_value;
  TypeAST returned_type;
};

struct TypedDefineStatementAST {
  std::string name;
  TypedExpressionAST value;
  TypeAST returned_type;
};

struct TypedRunStatementAST {
  std::vector<TypedExpressionAST> expressions;
};

struct TypedFunctionDeclarationAST;

using TypedFunctionBodyStatementAST = std::variant<std::unique_ptr<TypedReturnStatementAST>, std::unique_ptr<TypedDefineStatementAST>, std::unique_ptr<TypedFunctionDeclarationAST>>;
using TypedProcedureBodyStatementAST = std::variant<std::unique_ptr<TypedReturnStatementAST>, std::unique_ptr<TypedDefineStatementAST>, std::unique_ptr<TypedRunStatementAST>>;

struct TypedFunctionDeclarationAST {
  std::string function_name;
  TypeAST function_return_type;
  ParameterList parameters;
  std::vector<TypedFunctionBodyStatementAST> statements;
};

TypeAST get_type_matching_function_declaration(const TypedFunctionDeclarationAST& ast) {
  auto parameters = ast.parameters;
  auto return_type = std::make_shared<TypeAST>(ast.function_return_type);
  return FunctionType({
      parameters,
      return_type,
  });
}

struct TypedExternFunctionDeclarationAST {
  std::string function_name;
  TypeAST function_return_type;
  ParameterList parameters;
};

struct TypedProcedureDeclarationAST {
  std::string procedure_name;
  TypeAST procedure_return_type;
  ParameterList parameters;
  std::vector<TypedProcedureBodyStatementAST> statements;
};

struct TypedExternProcedureDeclarationAST {
  std::string procedure_name;
  TypeAST procedure_return_type;
  ParameterList parameters;
};

using TypedTopLevelStatementAST = std::variant<std::unique_ptr<TypedFunctionDeclarationAST>, std::unique_ptr<TypedDefineStatementAST>, std::unique_ptr<TypedExternFunctionDeclarationAST>, std::unique_ptr<TypedProcedureDeclarationAST>, std::unique_ptr<TypedExternProcedureDeclarationAST>>;

struct TypedProgramAST {
  std::vector<TypedTopLevelStatementAST> statements;
  std::map<std::string, const TypedFunctionDeclarationAST*> declared_functions;
  std::map<std::string, const TypedExternFunctionDeclarationAST*> declared_extern_functions;
  std::map<std::string, const TypedProcedureDeclarationAST*> declared_procedures;
  std::map<std::string, const TypedExternProcedureDeclarationAST*> declared_extern_procedures;
};

class VariableScope {
 public:
  static VariableScope InitializeGlobalScope() {
    return VariableScope(nullptr, "global-scope");
  }

  VariableScope create_nested_scope(std::string description) const {
    return VariableScope(this, description);
  }

  void add(std::string name, TypeAST type, std::string source) {
    if (lookup(name).has_value()) {
      std::cerr << "Error: name already in scope when trying to define: \"" << name << "\": " << pretty_print_type(type) << " from " << source << std::endl;
      std::cerr << "  defined from: " << lookup_source(name) << std::endl;
      assert(false);
    }
    sources[name] = source;
    types[name] = type;
    // std::cerr << "scope(" << description << ") add(" << name << ":" << pretty_print_type(type) << ") from " << source << std::endl;
  }

  std::optional<TypeAST> lookup(std::string name) const {
    auto maybe_value = types.find(name);
    if (maybe_value != types.end()) {
      return maybe_value->second;
    }
    if (parent_scope) {
      return parent_scope->lookup(name);
    } else {
      return std::nullopt;
    }
  }

 private:
  VariableScope(const VariableScope* parent_scope, std::string description) : parent_scope(parent_scope), description(description) {}

  const VariableScope* parent_scope;
  std::string description;
  std::map<std::string, std::string> sources;
  std::map<std::string, TypeAST> types;

  std::string lookup_source(std::string name) const {
    auto maybe_value = sources.find(name);
    if (maybe_value != sources.end()) {
      return maybe_value->second;
    }
    if (parent_scope) {
      return parent_scope->lookup_source(name);
    } else {
      std::cerr << "Failed to look up name: " << name << std::endl;
      assert(!"Internal error: could not find source when looking up name");
    }
  }
};

class Typer {
 public:
  enum class Mode {
    Quiet,
    Verbose,
  };

  static Typer Create(Mode mode = Mode::Quiet) {
    return Typer(mode);
  }

  std::unique_ptr<TypedProgramAST> TypeProgram(const std::unique_ptr<ProgramAST>& program) const {
    auto scope = VariableScope::InitializeGlobalScope();
    auto typed = std::make_unique<TypedProgramAST>();
    for (const auto& statement : program->statements) {
      typed->statements.emplace_back(TypeTopLevelStatement(statement, *typed, &scope));
    }
    return typed;
  }

 private:
  TypedTopLevelStatementAST TypeTopLevelStatement(const std::unique_ptr<TopLevelStatementAST>& ast, TypedProgramAST& program, VariableScope* scope) const {
    return std::visit(overloaded{
                          [&](const FunctionDeclarationAST& decl) {
                            if (mode == Mode::Verbose) {
                              std::cerr << "registering typed function: " << pretty_print_function_declaration_header(decl) << std::endl;
                            }
                            auto typed = TypeFunctionDeclaration(decl, program, scope);
                            program.declared_functions[typed->function_name] = typed.get();
                            return TypedTopLevelStatementAST(std::move(typed));
                          },
                          [&](const DefineStatementAST& def) {
                            auto typed = TypeDefineStatement(def, program, scope);
                            // auto sourcedesc = "top-level-define(" + typed->name + "):" + pretty_print_type(typed->returned_type);
                            // scope->add(typed->name, typed->returned_type, sourcedesc);
                            return TypedTopLevelStatementAST(std::move(typed));
                          },
                          [&](const ProcedureDeclarationAST& decl) {
                            if (mode == Mode::Verbose) {
                              std::cerr << "registering typed procedure: " << pretty_print_procedure_declaration_header(decl) << std::endl;
                            }
                            auto typed = TypeProcedureDeclaration(decl, program, scope);
                            program.declared_procedures[typed->procedure_name] = typed.get();
                            return TypedTopLevelStatementAST(std::move(typed));
                          },
                          [&](const ExternFunctionDeclarationAST& decl) {
                            if (mode == Mode::Verbose) {
                              std::cerr << "registering typed extern function: " << pretty_print_extern_function_declaration_header(decl) << std::endl;
                            }
                            auto typed = TypeExternFunctionDeclaration(decl, program, scope);
                            program.declared_extern_functions[typed->function_name] = typed.get();
                            return TypedTopLevelStatementAST(std::move(typed));
                          },
                          [&](const ExternProcedureDeclarationAST& decl) {
                            if (mode == Mode::Verbose) {
                              std::cerr << "registering typed extern procedure: " << pretty_print_extern_procedure_declaration_header(decl) << std::endl;
                            }
                            auto typed = TypeExternProcedureDeclaration(decl, program, scope);
                            program.declared_extern_procedures[typed->procedure_name] = typed.get();
                            return TypedTopLevelStatementAST(std::move(typed));
                          },
                      }  // namespace torpul
                      ,
                      *ast.get());
  }

  std::unique_ptr<TypedFunctionDeclarationAST> TypeFunctionDeclaration(const FunctionDeclarationAST& decl, const TypedProgramAST& program, const VariableScope* parent_scope) const {
    if (is_iotype(decl.function_return_type)) {
      std::cerr << "Error: function \"" << decl.function_name << "\" returns a procedure IO type: " << pretty_print_type(decl.function_return_type) << std::endl;
      assert(false);
    }
    VariableScope funcscope = parent_scope->create_nested_scope("function(" + decl.function_name + ")");
    for (const auto& [name, type] : decl.parameters) {
      auto sourcedesc = "function(" + decl.function_name + ")-param(" + name + "):" + pretty_print_type(type);
      funcscope.add(name, type, sourcedesc);
    }
    std::vector<TypedFunctionBodyStatementAST> statements;
    for (const auto& statement : decl.statements) {
      statements.emplace_back(TypeFunctionBodyStatement(statement, program, &funcscope));
    }
    if (statements.size() < 1) {
      std::cerr << "Error: function body of \"" << decl.function_name << "\" has no statements" << std::endl;
      assert(false);
    }
    for (int i = 0; i < (int)statements.size() - 1; i += 1) {
      const auto* maybe_statement = std::get_if<std::unique_ptr<TypedReturnStatementAST>>(&statements[i]);
      if (maybe_statement) {
        std::cerr << "Error: function \"" << decl.function_name << "\" has a return statement in the middle of the function body instead of at the end" << std::endl;
        assert(false);
      }
    }
    const auto* last_statement = std::get_if<std::unique_ptr<TypedReturnStatementAST>>(&statements.back());
    if (!last_statement) {
      std::cerr << "Error: function body of \"" << decl.function_name << "\" must end with a return statement" << std::endl;
      assert(false);
    }
    if (last_statement->get()->returned_type != decl.function_return_type) {
      std::cerr << "Error: type mismatch in function: got " << pretty_print_type(last_statement->get()->returned_type) << " returned for " << decl.function_name << "(...) but need " << pretty_print_type(decl.function_return_type) << std::endl;
      assert(false);
    }
    if (mode == Mode::Verbose) {
      std::cerr << "typing function declaration: " << decl.function_name << ": " << pretty_print_type(decl.function_return_type) << std::endl;
    }
    return std::make_unique<TypedFunctionDeclarationAST>(TypedFunctionDeclarationAST({
        decl.function_name,
        decl.function_return_type,
        decl.parameters,
        std::move(statements),
    }));
  }

  std::unique_ptr<TypedProcedureDeclarationAST> TypeProcedureDeclaration(const ProcedureDeclarationAST& decl, const TypedProgramAST& program, const VariableScope* parent_scope) const {
    if (!is_iotype(decl.procedure_return_type)) {
      std::cerr << "Error: procedure \"" << decl.procedure_name << "\" returns a function non-IO type: " << pretty_print_type(decl.procedure_return_type) << std::endl;
      assert(false);
    }
    VariableScope procscope = parent_scope->create_nested_scope("procedure(" + decl.procedure_name + ")");
    for (const auto& [name, type] : decl.parameters) {
      auto sourcedesc = "procedure(" + decl.procedure_name + ")-param(" + name + "):" + pretty_print_type(type);
      procscope.add(name, type, sourcedesc);
    }
    std::vector<TypedProcedureBodyStatementAST> statements;
    for (const auto& statement : decl.statements) {
      statements.emplace_back(TypeProcedureBodyStatement(statement, program, &procscope));
    }
    if (statements.size() < 1) {
      std::cerr << "Error: procedure body of \"" << decl.procedure_name << "\" has no statements" << std::endl;
      assert(false);
    }
    for (int i = 0; i < (int)statements.size() - 1; i += 1) {
      const auto* maybe_statement = std::get_if<std::unique_ptr<TypedReturnStatementAST>>(&statements[i]);
      if (maybe_statement) {
        std::cerr << "Error: procedure \"" << decl.procedure_name << "\" has a return statement in the middle of the procedure body instead of at the end" << std::endl;
        assert(false);
      }
    }
    const auto* last_statement = std::get_if<std::unique_ptr<TypedReturnStatementAST>>(&statements.back());
    if (!last_statement) {
      std::cerr << "Error: procedure body of \"" << decl.procedure_name << "\" must end with a return statement" << std::endl;
      assert(false);
    }
    // TODO: don't hackily wrap it as IO without any syntax!
    const auto* wrapped_return_type = std::get_if<IoType>(&decl.procedure_return_type);
    assert(wrapped_return_type != nullptr);
    if (last_statement->get()->returned_type != *wrapped_return_type->base_type) {
      std::cerr << "Error: type mismatch in procedure: got " << pretty_print_type(last_statement->get()->returned_type) << " returned for " << decl.procedure_name << "(...) but need " << pretty_print_type(*wrapped_return_type->base_type) << std::endl;
      assert(false);
    }
    if (mode == Mode::Verbose) {
      std::cerr << "typing procedure declaration: " << decl.procedure_name << ": " << pretty_print_type(decl.procedure_return_type) << std::endl;
    }
    return std::make_unique<TypedProcedureDeclarationAST>(TypedProcedureDeclarationAST({
        decl.procedure_name,
        decl.procedure_return_type,
        decl.parameters,
        std::move(statements),
    }));
  }

  std::unique_ptr<TypedExternFunctionDeclarationAST> TypeExternFunctionDeclaration(const ExternFunctionDeclarationAST& decl, const TypedProgramAST& program, const VariableScope* parent_scope) const {
    if (is_iotype(decl.function_return_type)) {
      std::cerr << "Error: function \"" << decl.function_name << "\" returns a procedure IO type: " << pretty_print_type(decl.function_return_type) << std::endl;
      assert(false);
    }
    VariableScope funcscope = parent_scope->create_nested_scope("extern-function(" + decl.function_name + ")");
    for (const auto& [name, type] : decl.parameters) {
      auto sourcedesc = "extern-function(" + decl.function_name + ")-param(" + name + "):" + pretty_print_type(type);
      funcscope.add(name, type, sourcedesc);
    }
    if (mode == Mode::Verbose) {
      std::cerr << "typing extern function declaration: " << decl.function_name << ": " << pretty_print_type(decl.function_return_type) << std::endl;
    }
    return std::make_unique<TypedExternFunctionDeclarationAST>(TypedExternFunctionDeclarationAST({
        decl.function_name,
        decl.function_return_type,
        decl.parameters,
    }));
  }

  std::unique_ptr<TypedExternProcedureDeclarationAST> TypeExternProcedureDeclaration(const ExternProcedureDeclarationAST& decl, const TypedProgramAST& program, const VariableScope* parent_scope) const {
    if (!is_iotype(decl.procedure_return_type)) {
      std::cerr << "Error: procedure \"" << decl.procedure_name << "\" returns a function non-IO type: " << pretty_print_type(decl.procedure_return_type) << std::endl;
      assert(false);
    }
    VariableScope procscope = parent_scope->create_nested_scope("extern-procedure(" + decl.procedure_name + ")");
    for (const auto& [name, type] : decl.parameters) {
      auto sourcedesc = "extern-procedure(" + decl.procedure_name + ")-param(" + name + "):" + pretty_print_type(type);
      procscope.add(name, type, sourcedesc);
    }
    if (mode == Mode::Verbose) {
      std::cerr << "typing extern procedure declaration: " << decl.procedure_name << ": " << pretty_print_type(decl.procedure_return_type) << std::endl;
    }
    return std::make_unique<TypedExternProcedureDeclarationAST>(TypedExternProcedureDeclarationAST({
        decl.procedure_name,
        decl.procedure_return_type,
        decl.parameters,
    }));
  }

  TypedFunctionBodyStatementAST TypeFunctionBodyStatement(const std::unique_ptr<FunctionBodyStatementAST>& ast, const TypedProgramAST& program, VariableScope* scope) const {
    return std::visit(overloaded{
                          [&](const ReturnStatementAST& ret) {
                            return TypedFunctionBodyStatementAST(TypeReturnStatement(ret, program, scope));
                          },
                          [&](const DefineStatementAST& def) {
                            return TypedFunctionBodyStatementAST(TypeDefineStatement(def, program, scope));
                          },
                          [&](const FunctionDeclarationAST& decl) {
                            auto typed = TypeFunctionDeclaration(decl, program, scope);
                            auto functype = get_type_matching_function_declaration(*typed);
                            if (mode == Mode::Verbose) {
                              std::cerr << "typing nested function declaration: " << typed->function_name << ": " << pretty_print_type(functype) << std::endl;
                            }
                            auto scopedesc = "nested-function(" + typed->function_name + ")";
                            scope->add(typed->function_name, functype, scopedesc);
                            return TypedFunctionBodyStatementAST(std::move(typed));
                          },
                      },
                      *ast.get());
  }

  TypedProcedureBodyStatementAST TypeProcedureBodyStatement(const std::unique_ptr<ProcedureBodyStatementAST>& ast, const TypedProgramAST& program, VariableScope* scope) const {
    return std::visit(overloaded{
                          [&](const ReturnStatementAST& ret) {
                            return TypedProcedureBodyStatementAST(TypeReturnStatement(ret, program, scope));
                          },
                          [&](const DefineStatementAST& def) {
                            return TypedProcedureBodyStatementAST(TypeDefineStatement(def, program, scope));
                          },
                          [&](const RunStatementAST& run) {
                            return TypedProcedureBodyStatementAST(TypeRunStatement(run, program, scope));
                          },
                      },
                      *ast.get());
  }

  std::unique_ptr<TypedReturnStatementAST> TypeReturnStatement(const ReturnStatementAST& ret, const TypedProgramAST& program, const VariableScope* scope) const {
    auto expr = TypeExpression(ret.return_value, program, scope);
    auto type = expr.type;
    if (mode == Mode::Verbose) {
      std::cerr << "typing return statement: " << pretty_print_type(type) << std::endl;
    }
    return std::make_unique<TypedReturnStatementAST>(TypedReturnStatementAST({std::move(expr), type}));
  }

  std::unique_ptr<TypedDefineStatementAST> TypeDefineStatement(const DefineStatementAST& def, const TypedProgramAST& program, VariableScope* scope) const {
    auto expr = TypeExpression(def.value, program, scope);
    auto type = expr.type;
    auto sourcedesc = "define(" + def.name + "):" + pretty_print_type(type);
    scope->add(def.name, type, sourcedesc);
    if (mode == Mode::Verbose) {
      std::cerr << "typing define statement: " << pretty_print_type(type) << std::endl;
    }
    return std::make_unique<TypedDefineStatementAST>(TypedDefineStatementAST({def.name, std::move(expr), type}));
  }

  std::unique_ptr<TypedRunStatementAST> TypeRunStatement(const RunStatementAST& run, const TypedProgramAST& program, const VariableScope* scope) const {
    std::vector<TypedExpressionAST> expressions;
    for (const auto& expr : run.expressions) {
      expressions.emplace_back(TypeExpression(expr, program, scope));
    }
    if (mode == Mode::Verbose) {
      std::cerr << "typing run statement" << std::endl;
    }
    return std::make_unique<TypedRunStatementAST>(TypedRunStatementAST({std::move(expressions)}));
  }

  TypedExpressionAST TypeExpression(const std::unique_ptr<ExpressionAST>& ast, const TypedProgramAST& program, const VariableScope* scope) const {
    return std::visit(overloaded{
                          [&](const ExpressionVariableAST& var) {
                            auto expr = TypeVariableExpression(var, program, scope);
                            auto type = expr->type;
                            return TypedExpressionAST({std::move(expr), type});
                          },
                          [&](const ExpressionFunctionCallAST& call) {
                            auto expr = TypeFunctionCallExpression(call, program, scope);
                            auto type = expr->type;
                            return TypedExpressionAST({std::move(expr), type});
                          },
                          [&](const ExpressionProcedureCallAST& call) {
                            auto expr = TypeProcedureCallExpression(call, program, scope);
                            auto type = expr->type;
                            return TypedExpressionAST({std::move(expr), type});
                          },
                          [&](const ExpressionNumberAST& number) {
                            auto expr = TypeNumberExpression(number, program, scope);
                            auto type = expr->type;
                            return TypedExpressionAST({std::move(expr), type});
                          },
                          [&](const ExpressionTruthValueAST& truth_value) {
                            auto expr = TypeTruthValueExpression(truth_value, program, scope);
                            auto type = expr->type;
                            return TypedExpressionAST({std::move(expr), type});
                          },
                          [&](const ExpressionIfThenElseAST& ifthenelse) {
                            auto expr = TypeIfThenElseExpression(ifthenelse, program, scope);
                            auto type = expr->type;
                            return TypedExpressionAST({std::move(expr), type});
                          },
                      },
                      *ast.get());
  }

  std::unique_ptr<TypedExpressionVariableAST> TypeVariableExpression(const ExpressionVariableAST& var, const TypedProgramAST& program, const VariableScope* scope) const {
    if (mode == Mode::Verbose) {
      std::cerr << "typing variable: \"" << var.variable_name << "\"" << std::endl;
    }
    auto maybe_value = scope->lookup(var.variable_name);
    if (!maybe_value.has_value()) {
      std::cerr << "Error: variable \"" << var.variable_name << "\" not in scope" << std::endl;
      assert(false);
    }
    auto type = maybe_value.value();
    return std::make_unique<TypedExpressionVariableAST>(TypedExpressionVariableAST({var.variable_name, type}));
  }

  std::optional<std::tuple<const std::string, const std::map<std::string, TypeAST>, TypeAST>> lookup_function(const std::string& function_name, const TypedProgramAST& program) const {
    const auto func = program.declared_functions.find(function_name);
    if (func != program.declared_functions.end()) {
      return std::make_optional<std::tuple<const std::string&, const std::map<std::string, TypeAST>&, TypeAST>>({
          func->second->function_name,
          func->second->parameters,
          func->second->function_return_type,
      });
    }
    const auto extfunc = program.declared_extern_functions.find(function_name);
    if (extfunc != program.declared_extern_functions.end()) {
      return std::make_optional<std::tuple<const std::string&, const std::map<std::string, TypeAST>&, TypeAST>>({
          extfunc->second->function_name,
          extfunc->second->parameters,
          extfunc->second->function_return_type,
      });
    }
    return std::nullopt;
  }

  std::optional<std::tuple<const std::string&, const std::map<std::string, TypeAST>&, TypeAST>> lookup_procedure(const std::string& function_name, const TypedProgramAST& program) const {
    const auto proc = program.declared_procedures.find(function_name);
    if (proc != program.declared_procedures.end()) {
      return std::make_optional<std::tuple<const std::string&, const std::map<std::string, TypeAST>&, TypeAST>>({
          proc->second->procedure_name,
          proc->second->parameters,
          proc->second->procedure_return_type,
      });
    }
    const auto extproc = program.declared_extern_procedures.find(function_name);
    if (extproc != program.declared_extern_procedures.end()) {
      return std::make_optional<std::tuple<const std::string&, const std::map<std::string, TypeAST>&, TypeAST>>({
          extproc->second->procedure_name,
          extproc->second->parameters,
          extproc->second->procedure_return_type,
      });
    }
    return std::nullopt;
  }

  std::unique_ptr<TypedExpressionFunctionCallAST> TypeFunctionCallExpression(const ExpressionFunctionCallAST& call, const TypedProgramAST& program, const VariableScope* scope) const {
    if (mode == Mode::Verbose) {
      std::cerr << "typing function call: " << call.function_name << std::endl;
    }
    const auto& [funcname, funcparams, funcreturntype] = ([&]() -> std::tuple<const std::string, const std::map<std::string, TypeAST>, TypeAST> {
      auto maybe_function = lookup_function(call.function_name, program);
      if (maybe_function.has_value()) {
        return maybe_function.value();
      }

      auto result = scope->lookup(call.function_name);
      if (result.has_value()) {
        auto* func = std::get_if<FunctionType>(&result.value());
        assert(func);
        // std::cerr << "TODO: do something with the looked-up " << call.function_name << ": " << pretty_print_type(*func) << std::endl;
        TypeAST t = *func->return_type;
        auto params = func->parameters;
        return std::make_tuple<const std::string, const std::map<std::string, TypeAST>, TypeAST>(std::string(call.function_name), std::move(params), std::move(t));
      } else {
        std::cerr << "Huh, it's really not in scope :/" << std::endl;
      }

      std::cerr << "Error: function \"" << call.function_name << "\" not in scope" << std::endl;
      if (lookup_procedure(call.function_name, program)) {
        std::cerr << "  " << call.function_name << "(...) is a procedure" << std::endl;
      }
      assert(false);
    })();
    std::map<std::string, TypedExpressionAST> parameter_values;
    for (const auto& [name, need_type] : funcparams) {
      // std::cerr << "DEBUG checking funcparam name: " << name << " and type " << pretty_print_type(need_type) << std::endl;
      const auto ast = call.arguments.find(name);
      if (ast == call.arguments.end()) {
        std::cerr << "Error: no function call argument provided for " << call.function_name << "(...) parameter: \"" << name << "\"" << std::endl;
        assert(false);
      }
      auto value = TypeExpression(ast->second, program, scope);
      auto got_type = value.type;
      if (got_type != need_type) {
        std::cerr << "Error: type mismatch in function call argument: got " << pretty_print_type(got_type) << " but need " << pretty_print_type(need_type) << " for " << call.function_name << "(...) parameter " << name << std::endl;
        assert(false);
      }
      parameter_values[name] = std::move(value);
    }
    for (const auto& [name, expr] : call.arguments) {
      const auto param = funcparams.find(name);
      if (param == funcparams.end()) {
        std::cerr << "Error: unknown function argument provided in " << call.function_name << "(...) call: \"" << name << "\"" << std::endl;
        assert(false);
      }
    }
    return std::make_unique<TypedExpressionFunctionCallAST>(TypedExpressionFunctionCallAST({
        funcname,
        std::move(parameter_values),
        funcreturntype,
    }));
  }

  std::unique_ptr<TypedExpressionProcedureCallAST> TypeProcedureCallExpression(const ExpressionProcedureCallAST& call, const TypedProgramAST& program, const VariableScope* scope) const {
    if (mode == Mode::Verbose) {
      std::cerr << "typing procedure call: " << call.procedure_name << std::endl;
    }
    const auto maybe_procedure = lookup_procedure(call.procedure_name, program);
    if (!maybe_procedure.has_value()) {
      std::cerr << "Error: procedure \"" << call.procedure_name << "\" not in scope" << std::endl;
      if (lookup_function(call.procedure_name, program)) {
        std::cerr << "  " << call.procedure_name << "(...) is a function" << std::endl;
      }
      assert(false);
    }
    const auto& [procname, procparams, procreturntype] = maybe_procedure.value();
    std::map<std::string, TypedExpressionAST> parameter_values;
    for (const auto& [name, need_type] : procparams) {
      const auto ast = call.arguments.find(name);
      if (ast == call.arguments.end()) {
        std::cerr << "Error: no procedure call argument provided for " << call.procedure_name << "(...) parameter: \"" << name << "\"" << std::endl;
        assert(false);
      }
      auto value = TypeExpression(ast->second, program, scope);
      auto got_type = value.type;
      if (got_type != need_type) {
        std::cerr << "Error: type mismatch in procedure call argument: got " << pretty_print_type(got_type) << " but need " << pretty_print_type(need_type) << " for " << call.procedure_name << "(...) parameter " << name << std::endl;
        assert(false);
      }
      parameter_values[name] = std::move(value);
    }
    for (const auto& [name, expr] : call.arguments) {
      const auto param = procparams.find(name);
      if (param == procparams.end()) {
        std::cerr << "Error: unknown procedure argument provided in " << call.procedure_name << "(...) call: \"" << name << "\"" << std::endl;
        assert(false);
      }
    }
    return std::make_unique<TypedExpressionProcedureCallAST>(TypedExpressionProcedureCallAST({
        procname,
        std::move(parameter_values),
        procreturntype,
    }));
  }

  std::unique_ptr<TypedExpressionNumberAST> TypeNumberExpression(const ExpressionNumberAST& number, const TypedProgramAST& program, const VariableScope* scope) const {
    if (mode == Mode::Verbose) {
      std::cerr << "typing number: " << number.number_string << ": " << pretty_print_type(number.number_type) << std::endl;
    }
    return std::make_unique<TypedExpressionNumberAST>(TypedExpressionNumberAST({number.number_string, number.number_type}));
  }

  std::unique_ptr<TypedExpressionTruthValueAST> TypeTruthValueExpression(const ExpressionTruthValueAST& truth_value, const TypedProgramAST& program, const VariableScope* scope) const {
    if (mode == Mode::Verbose) {
      std::cerr << "typing truth value: " << truth_value.truth_value << std::endl;
    }
    return std::make_unique<TypedExpressionTruthValueAST>(TypedExpressionTruthValueAST({truth_value.truth_value, TruthValueType{}}));
  }

  std::unique_ptr<TypedExpressionIfThenElseAST> TypeIfThenElseExpression(const ExpressionIfThenElseAST& ifthenelse, const TypedProgramAST& program, const VariableScope* scope) const {
    if (mode == Mode::Verbose) {
      std::cerr << "typing ifthenelse" << std::endl;
    }
    auto condition = TypeExpression(ifthenelse.condition, program, scope);
    if (condition.type != TypeAST(TruthValueType{})) {
      std::cerr << "Error: type mismatch in if condition: got " << pretty_print_type(condition.type) << " but need " << pretty_print_type(TypeAST(TruthValueType{})) << std::endl;
      assert(false);
    }
    auto then_clause = TypeExpression(ifthenelse.then_clause, program, scope);
    auto else_clause = TypeExpression(ifthenelse.else_clause, program, scope);
    if (then_clause.type != else_clause.type) {
      std::cerr << "Error: type mismatch in if expression: got " << pretty_print_type(then_clause.type) << " in then-clause which doesn't match " << pretty_print_type(else_clause.type) << " in else-clause" << std::endl;
      assert(false);
    }
    auto type = then_clause.type;
    return std::make_unique<TypedExpressionIfThenElseAST>(TypedExpressionIfThenElseAST({
        std::move(condition),
        std::move(then_clause),
        std::move(else_clause),
        type,
    }));
  }

  Typer(Mode mode) : mode(mode) {}
  const Mode mode;
};

std::string pretty_print_typed_function_declaration_header(const TypedFunctionDeclarationAST& decl) {
  return decl.function_name + "(" + pretty_print_parameter_list(decl.parameters) + "): " + pretty_print_type(decl.function_return_type);
}

std::string pretty_print_typed_extern_function_declaration_header(const TypedExternFunctionDeclarationAST& decl) {
  return decl.function_name + "(" + pretty_print_parameter_list(decl.parameters) + "): " + pretty_print_type(decl.function_return_type);
}

std::string pretty_print_typed_procedure_declaration_header(const TypedProcedureDeclarationAST& decl) {
  return decl.procedure_name + "(" + pretty_print_parameter_list(decl.parameters) + "): " + pretty_print_type(decl.procedure_return_type);
}

std::string pretty_print_typed_extern_procedure_declaration_header(const TypedExternProcedureDeclarationAST& decl) {
  return decl.procedure_name + "(" + pretty_print_parameter_list(decl.parameters) + "): " + pretty_print_type(decl.procedure_return_type);
}

}  // namespace torpul

#endif
