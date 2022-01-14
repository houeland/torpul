#ifndef TORPUL_LLVM_CODEGEN_H
#define TORPUL_LLVM_CODEGEN_H

#include <cassert>

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm_jit.h"
#include "typer.h"

namespace torpul {

class LlvmCodegen {
 public:
  enum class Mode {
    Quiet,
    Verbose,
  };

  static LlvmCodegen Create(Mode mode = Mode::Quiet) {
    return LlvmCodegen(mode);
  }

  void doStuff(const TypedProgramAST& program) {
    std::cerr << std::endl
              << std::endl
              << "  ==  doStuff  ==  " << std::endl;
    std::cerr << "starting codegen..." << std::endl;
    auto jit = LlvmJit::Create();

    std::cerr << "initialized codegen" << std::endl;
    for (const auto& statement : program.statements) {
      auto context_holder = std::make_unique<llvm::LLVMContext>();
      auto& llvm_context = *context_holder;
      auto llvm_module = std::make_unique<llvm::Module>("torpul module", llvm_context);
      llvm_module->setDataLayout(jit.data_layout);
      std::cerr << "compiling statement" << std::endl;
      codegen_function(statement, program, llvm_context, llvm_module);
      jit.AddModule(llvm::orc::ThreadSafeModule(std::move(llvm_module), std::move(context_holder)));
    }
  }

  void compileProgram(const TypedProgramAST& program) {
    auto context_holder = std::make_unique<llvm::LLVMContext>();
    auto& llvm_context = *context_holder;
    auto llvm_module = std::make_unique<llvm::Module>("torpul main module", llvm_context);
    auto llvm_target_triple = llvm::sys::getDefaultTargetTriple();
    llvm_module->setTargetTriple(llvm_target_triple);
    std::string error_msg;
    auto llvm_target = llvm::TargetRegistry::lookupTarget(llvm_target_triple, error_msg);
    if (!llvm_target) {
      llvm::errs() << error_msg;
      return;
    }
    auto llvm_cpu = "generic";
    auto llvm_features = "";
    llvm::TargetOptions llvm_target_options;
    auto llvm_reloc_model = llvm::Optional<llvm::Reloc::Model>();
    auto llvm_target_machine = llvm_target->createTargetMachine(llvm_target_triple, llvm_cpu, llvm_features, llvm_target_options, llvm_reloc_model);
    llvm_module->setDataLayout(llvm_target_machine->createDataLayout());

    for (const auto& statement : program.statements) {
      codegen_function(statement, program, llvm_context, llvm_module);
    }

    std::string output_filename = "build/output.o";
    std::error_code write_error_code;
    llvm::raw_fd_ostream dest(output_filename, write_error_code, llvm::sys::fs::OF_None);

    if (write_error_code) {
      std::cerr << "Could not open file: " << write_error_code.message() << std::endl;
    }

    llvm::legacy::PassManager pass;
    auto llvm_file_type = llvm::CGFT_ObjectFile;

    if (llvm_target_machine->addPassesToEmitFile(pass, dest, nullptr, llvm_file_type)) {
      std::cerr << "Target machine can't emit a file of this type: " << llvm_file_type << std::endl;
      return;
    }

    pass.run(*llvm_module);
    dest.flush();

    std::cerr << "Wrote output file: " << output_filename << std::endl;
  }

 private:
  llvm::Function* codegen_function(const TypedTopLevelStatementAST& ast, const TypedProgramAST& program, llvm::LLVMContext& llvm_context, std::unique_ptr<llvm::Module>& llvm_module) {
    auto old_indent = indent_prefix;
    indent_prefix += ">> ";
    auto* function = std::visit(overloaded{
                                    [&](const std::unique_ptr<TypedFunctionDeclarationAST>& decl) {
                                      if (mode == Mode::Verbose) {
                                        std::cerr << indent_prefix << "compiling function: " << pretty_print_typed_function_declaration_header(*decl) << std::endl;
                                      }

                                      auto builder = std::make_unique<llvm::IRBuilder<>>(llvm_context);
                                      auto* llvm_function = make_function(*decl, llvm_context, llvm_module);

                                      auto* bb = llvm::BasicBlock::Create(llvm_context, "entry", llvm_function);
                                      builder->SetInsertPoint(bb);

                                      // std::cerr << "...created basic block:" << bb << std::endl;

                                      std::map<std::string, llvm::Value*> variable_lookup;
                                      for (auto& arg : llvm_function->args()) {
                                        variable_lookup[std::string(arg.getName())] = &arg;
                                      }

                                      for (const auto& statement : decl->statements) {
                                        std::visit(overloaded{
                                                       [&](const std::unique_ptr<TypedReturnStatementAST>& ret) {
                                                         auto* retval = codegen_value(ret->return_value, llvm_context, llvm_module, program, builder, variable_lookup);
                                                         builder->CreateRet(retval);
                                                       },
                                                       [&](const std::unique_ptr<TypedDefineStatementAST>& def) {
                                                         variable_lookup[def->name] = codegen_value(def->value, llvm_context, llvm_module, program, builder, variable_lookup);
                                                       },
                                                   },
                                                   statement);
                                      }

                                      llvm::verifyFunction(*llvm_function);

                                      std::cerr << "Defined function:" << std::endl;
                                      llvm_function->print(llvm::errs());
                                      std::cerr << std::endl;

                                      auto llvm_fpm = std::make_unique<llvm::legacy::FunctionPassManager>(llvm_module.get());
                                      llvm_fpm->add(llvm::createInstructionCombiningPass());
                                      llvm_fpm->add(llvm::createReassociatePass());
                                      llvm_fpm->add(llvm::createGVNPass());
                                      llvm_fpm->add(llvm::createCFGSimplificationPass());
                                      llvm_fpm->doInitialization();
                                      // std::cerr << "optimizing..." << std::endl;
                                      if (llvm_fpm->run(*llvm_function)) {
                                        std::cerr << "=== after optimizing: ===" << std::endl;
                                        llvm_function->print(llvm::errs());
                                        std::cerr << std::endl;
                                      };
                                      // std::cerr << "...done optimizing" << std::endl;
                                      return static_cast<llvm::Function*>(llvm_function);
                                    },
                                    [&](const std::unique_ptr<TypedExternFunctionDeclarationAST>& decl) {
                                      if (mode == Mode::Verbose) {
                                        std::cerr << indent_prefix << "compiling extern function: " << pretty_print_typed_extern_function_declaration_header(*decl) << std::endl;
                                      }

                                      auto builder = std::make_unique<llvm::IRBuilder<>>(llvm_context);
                                      auto* llvm_function = make_extern_function(*decl, llvm_context, llvm_module);
                                      std::cerr << "Defined extern function:" << std::endl;
                                      llvm_function->print(llvm::errs());
                                      std::cerr << std::endl;
                                      return static_cast<llvm::Function*>(llvm_function);
                                    },
                                    [&](const std::unique_ptr<TypedProcedureDeclarationAST>& decl) {
                                      if (mode == Mode::Verbose) {
                                        std::cerr << indent_prefix << "compiling procedure: " << pretty_print_typed_procedure_declaration_header(*decl) << std::endl;
                                      }

                                      auto builder = std::make_unique<llvm::IRBuilder<>>(llvm_context);
                                      auto* llvm_function = make_procedure(*decl, llvm_context, llvm_module);

                                      auto* bb = llvm::BasicBlock::Create(llvm_context, "entry", llvm_function);
                                      builder->SetInsertPoint(bb);

                                      // std::cerr << "...created basic block:" << bb << std::endl;

                                      std::map<std::string, llvm::Value*> variable_lookup;
                                      for (auto& arg : llvm_function->args()) {
                                        variable_lookup[std::string(arg.getName())] = &arg;
                                      }

                                      for (const auto& statement : decl->statements) {
                                        std::visit(overloaded{
                                                       [&](const std::unique_ptr<TypedReturnStatementAST>& ret) {
                                                         auto* retval = codegen_value(ret->return_value, llvm_context, llvm_module, program, builder, variable_lookup);
                                                         builder->CreateRet(retval);
                                                       },
                                                       [&](const std::unique_ptr<TypedDefineStatementAST>& def) {
                                                         variable_lookup[def->name] = codegen_value(def->value, llvm_context, llvm_module, program, builder, variable_lookup);
                                                       },
                                                       [&](const std::unique_ptr<TypedRunStatementAST>& run) {
                                                         codegen_runstatement(run, llvm_context, llvm_module, program, builder, variable_lookup);
                                                       },
                                                   },
                                                   statement);
                                      }

                                      llvm::verifyFunction(*llvm_function);

                                      std::cerr << "Defined procedure:" << std::endl;
                                      llvm_function->print(llvm::errs());
                                      std::cerr << std::endl;

                                      auto llvm_fpm = std::make_unique<llvm::legacy::FunctionPassManager>(llvm_module.get());
                                      llvm_fpm->add(llvm::createInstructionCombiningPass());
                                      llvm_fpm->add(llvm::createReassociatePass());
                                      llvm_fpm->add(llvm::createGVNPass());
                                      llvm_fpm->add(llvm::createCFGSimplificationPass());
                                      llvm_fpm->doInitialization();
                                      // std::cerr << "optimizing..." << std::endl;
                                      if (llvm_fpm->run(*llvm_function)) {
                                        std::cerr << "=== after optimizing: ===" << std::endl;
                                        llvm_function->print(llvm::errs());
                                        std::cerr << std::endl;
                                      };
                                      // std::cerr << "...done optimizing" << std::endl;
                                      return static_cast<llvm::Function*>(llvm_function);
                                    },
                                    [&](const std::unique_ptr<TypedExternProcedureDeclarationAST>& decl) {
                                      if (mode == Mode::Verbose) {
                                        std::cerr << indent_prefix << "compiling extern procedure: " << pretty_print_typed_extern_procedure_declaration_header(*decl) << std::endl;
                                      }

                                      auto builder = std::make_unique<llvm::IRBuilder<>>(llvm_context);
                                      auto* llvm_function = make_extern_procedure(*decl, llvm_context, llvm_module);
                                      std::cerr << "Defined extern procedure:" << std::endl;
                                      llvm_function->print(llvm::errs());
                                      std::cerr << std::endl;
                                      return static_cast<llvm::Function*>(llvm_function);
                                    },
                                },
                                ast);
    indent_prefix = old_indent;
    return function;
  }

  llvm::Value* codegen_value(const TypedExpressionAST& ast, llvm::LLVMContext& llvm_context, std::unique_ptr<llvm::Module>& llvm_module, const TypedProgramAST& program, std::unique_ptr<llvm::IRBuilder<>>& builder, std::map<std::string, llvm::Value*>& variable_lookup) {
    auto old_indent = indent_prefix;
    indent_prefix += "  ";
    auto* value = std::visit(overloaded{
                                 [&](const std::unique_ptr<TypedExpressionNumberAST>& decl) {
                                   if (mode == Mode::Verbose) {
                                     std::cerr << indent_prefix << "compiling number: " << decl->number_string << std::endl;
                                   }
                                   llvm::APFloat apfloat_value(0.0);
                                   auto result = apfloat_value.convertFromString(decl->number_string, llvm::RoundingMode::NearestTiesToEven);
                                   if (!result) {
                                     llvm::errs() << "error converting number: " << result.takeError();
                                     std::cerr << std::endl;
                                     assert(false);
                                   } else if (result.get() != llvm::APFloatBase::opStatus::opOK) {
                                     std::cerr << "floating point exception while converting number:" << result.get() << std::endl;
                                     assert(false);
                                   } else {
                                     return static_cast<llvm::Value*>(llvm::ConstantFP::get(llvm_context, apfloat_value));
                                   }
                                 },
                                 [&](const std::unique_ptr<TypedExpressionTruthValueAST>& decl) {
                                   if (mode == Mode::Verbose) {
                                     std::cerr << indent_prefix << "compiling truth-value: " << decl->truth_value << std::endl;
                                   }
                                   return static_cast<llvm::Value*>(llvm::ConstantFP::get(llvm_context, llvm::APFloat(decl->truth_value ? 1.0 : 0.0)));
                                 },
                                 [&](const std::unique_ptr<TypedExpressionVariableAST>& decl) {
                                   if (mode == Mode::Verbose) {
                                     std::cerr << indent_prefix << "compiling variable lookup: " << decl->variable_name << std::endl;
                                   }
                                   auto* value = variable_lookup.find(decl->variable_name)->second;
                                   return value;
                                 },
                                 [&](const std::unique_ptr<TypedExpressionFunctionCallAST>& decl) {
                                   if (mode == Mode::Verbose) {
                                     std::cerr << indent_prefix << "compiling function call: " << decl->function_name << std::endl;
                                   }
                                   auto* func = llvm_module->getFunction(decl->function_name);
                                   if (!func) {
                                     const auto* function_decl = program.declared_functions.find(decl->function_name)->second;
                                     func = make_function(*function_decl, llvm_context, llvm_module);
                                   }
                                   std::vector<llvm::Value*> argvalues;
                                   for (const auto& [name, argument_ast] : decl->parameter_values) {
                                     auto* value = codegen_value(argument_ast, llvm_context, llvm_module, program, builder, variable_lookup);
                                     argvalues.push_back(value);
                                   }
                                   return static_cast<llvm::Value*>(builder->CreateCall(func, argvalues, "call_" + decl->function_name + "_result"));
                                 },
                                 [&](const std::unique_ptr<TypedExpressionIfThenElseAST>& decl) {
                                   if (mode == Mode::Verbose) {
                                     std::cerr << indent_prefix << "compiling if-then-else" << std::endl;
                                   }
                                   auto* raw_condition = codegen_value(decl->condition, llvm_context, llvm_module, program, builder, variable_lookup);
                                   auto* condition = builder->CreateFCmpONE(raw_condition, llvm::ConstantFP::get(llvm_context, llvm::APFloat(0.0)), "if_condition");

                                   auto* current_parent_function = builder->GetInsertBlock()->getParent();

                                   auto* then_bb = llvm::BasicBlock::Create(llvm_context, "then_clause", current_parent_function);
                                   auto* else_bb = llvm::BasicBlock::Create(llvm_context, "else_clause", current_parent_function);
                                   auto* endif_bb = llvm::BasicBlock::Create(llvm_context, "endif", current_parent_function);

                                   builder->CreateCondBr(condition, then_bb, else_bb);

                                   builder->SetInsertPoint(then_bb);
                                   auto* then_value = codegen_value(decl->then_clause, llvm_context, llvm_module, program, builder, variable_lookup);
                                   builder->CreateBr(endif_bb);

                                   builder->SetInsertPoint(else_bb);
                                   auto* else_value = codegen_value(decl->else_clause, llvm_context, llvm_module, program, builder, variable_lookup);
                                   builder->CreateBr(endif_bb);

                                   builder->SetInsertPoint(endif_bb);

                                   auto* endif_phi = builder->CreatePHI(llvm::Type::getDoubleTy(llvm_context), 2, "endif_phi");
                                   endif_phi->addIncoming(then_value, then_bb);
                                   endif_phi->addIncoming(else_value, else_bb);

                                   return static_cast<llvm::Value*>(endif_phi);
                                 },
                             },
                             ast.value);
    indent_prefix = old_indent;
    return value;
  }

  void codegen_runstatement(const std::unique_ptr<TypedRunStatementAST>& run, llvm::LLVMContext& llvm_context, std::unique_ptr<llvm::Module>& llvm_module, const TypedProgramAST& program, std::unique_ptr<llvm::IRBuilder<>>& builder, std::map<std::string, llvm::Value*>& variable_lookup) {
    for (const auto& do_statement : run->expressions) {
      codegen_value(do_statement, llvm_context, llvm_module, program, builder, variable_lookup);
    }
  }

  llvm::Function* make_function(const TypedFunctionDeclarationAST& decl, llvm::LLVMContext& llvm_context, std::unique_ptr<llvm::Module>& llvm_module) {
    // TODO: Support proper type representations rather than just doubles
    std::vector<llvm::Type*> types(decl.parameters.size(), llvm::Type::getDoubleTy(llvm_context));
    auto* function_type = llvm::FunctionType::get(llvm::Type::getDoubleTy(llvm_context), types, false);
    auto* func = llvm::Function::Create(function_type, llvm::Function::ExternalLinkage, decl.function_name, llvm_module.get());

    std::vector<std::string> param_names;
    for (const auto& [name, type] : decl.parameters) {
      param_names.push_back(name);
    }
    unsigned idx = 0;
    for (auto& arg : func->args()) {
      arg.setName(param_names[idx++]);
    }

    func->setWillReturn();
    func->setNoSync();
    func->setDoesNotThrow();
    func->setDoesNotAccessMemory();
    func->setSpeculatable();
    return func;
  }

  llvm::Function* make_procedure(const TypedProcedureDeclarationAST& decl, llvm::LLVMContext& llvm_context, std::unique_ptr<llvm::Module>& llvm_module) {
    // TODO: Support proper type representations rather than just doubles
    std::vector<llvm::Type*> types(decl.parameters.size(), llvm::Type::getDoubleTy(llvm_context));
    auto* function_type = llvm::FunctionType::get(llvm::Type::getDoubleTy(llvm_context), types, false);
    auto* func = llvm::Function::Create(function_type, llvm::Function::ExternalLinkage, decl.procedure_name, llvm_module.get());

    std::vector<std::string> param_names;
    for (const auto& [name, type] : decl.parameters) {
      param_names.push_back(name);
    }
    unsigned idx = 0;
    for (auto& arg : func->args()) {
      arg.setName(param_names[idx++]);
    }

    func->setNoSync();
    func->setDoesNotThrow();
    func->setDoesNotAccessMemory();
    return func;
  }
  llvm::Function* make_extern_function(const TypedExternFunctionDeclarationAST& decl, llvm::LLVMContext& llvm_context, std::unique_ptr<llvm::Module>& llvm_module) {
    // TODO: Support proper type representations rather than just doubles
    std::vector<llvm::Type*> types(decl.parameters.size(), llvm::Type::getDoubleTy(llvm_context));
    auto* function_type = llvm::FunctionType::get(llvm::Type::getDoubleTy(llvm_context), types, false);
    auto* func = llvm::Function::Create(function_type, llvm::Function::ExternalLinkage, decl.function_name, llvm_module.get());

    std::vector<std::string> param_names;
    for (const auto& [name, type] : decl.parameters) {
      param_names.push_back(name);
    }
    unsigned idx = 0;
    for (auto& arg : func->args()) {
      arg.setName(param_names[idx++]);
    }

    func->setWillReturn();
    func->setNoSync();
    func->setDoesNotThrow();
    func->setDoesNotAccessMemory();
    func->setSpeculatable();
    return func;
  }

  llvm::Function* make_extern_procedure(const TypedExternProcedureDeclarationAST& decl, llvm::LLVMContext& llvm_context, std::unique_ptr<llvm::Module>& llvm_module) {
    // TODO: Support proper type representations rather than just doubles
    std::vector<llvm::Type*> types(decl.parameters.size(), llvm::Type::getDoubleTy(llvm_context));
    auto* function_type = llvm::FunctionType::get(llvm::Type::getDoubleTy(llvm_context), types, false);
    auto* func = llvm::Function::Create(function_type, llvm::Function::ExternalLinkage, decl.procedure_name, llvm_module.get());

    std::vector<std::string> param_names;
    for (const auto& [name, type] : decl.parameters) {
      param_names.push_back(name);
    }
    unsigned idx = 0;
    for (auto& arg : func->args()) {
      arg.setName(param_names[idx++]);
    }

    func->setNoSync();
    func->setDoesNotThrow();
    func->setDoesNotAccessMemory();
    return func;
  }

  LlvmCodegen(Mode mode) : mode(mode) {}
  const Mode mode;
  std::string indent_prefix = "";
};

}  // namespace torpul

#endif
