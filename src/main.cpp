#include "lexer.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm_codegen.h"
#include "llvm_jit.h"
#include "parser.h"
#include "pretty_printer.h"
#include "typer.h"

int main() {
  torpul::Lexer lexer = torpul::Lexer::Create(torpul::Lexer::Mode::Quiet);
  torpul::Parser parser = torpul::Parser::Create(&lexer, torpul::Parser::Mode::Quiet);
  const auto parsed_program = parser.ParseProgram();
  torpul::pretty_print(parsed_program);

  const torpul::Typer typer = torpul::Typer::Create(torpul::Typer::Mode::Quiet);
  const auto typed_program = typer.TypeProgram(parsed_program);

  // TODO: move out of main?
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  torpul::LlvmCodegen codegen = torpul::LlvmCodegen::Create(torpul::LlvmCodegen::Mode::Verbose);
  // codegen.doStuff(*typed_program);
  codegen.compileProgram(*typed_program);
}
