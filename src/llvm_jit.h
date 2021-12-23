#ifndef TORPUL_LLVM_JIT_H
#define TORPUL_LLVM_JIT_H

#include <cassert>

#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/Support/Error.h"

namespace torpul {

class LlvmJit {
 public:
  enum class Mode {
    Quiet,
    Verbose,
  };

  static LlvmJit Create(Mode mode = Mode::Quiet) {
    auto epc = llvm::orc::SelfExecutorProcessControl::Create();
    if (!epc) {
      llvm::errs() << "epc failed:" << epc.takeError();
    }
    assert(epc);

    auto es = std::make_unique<llvm::orc::ExecutionSession>(std::move(*epc));

    llvm::orc::JITTargetMachineBuilder jtmb(es->getExecutorProcessControl().getTargetTriple());

    auto dl = jtmb.getDefaultDataLayoutForTarget();
    if (!dl) {
      llvm::errs() << "dl failed:" << dl.takeError();
    }
    assert(dl);

    return LlvmJit(mode, std::move(es), std::move(jtmb), std::move(*dl));
  }

  void AddModule(llvm::orc::ThreadSafeModule TSM) {
    llvm::Error err = compile_layer.add(main_jit_dylib.getDefaultResourceTracker(), std::move(TSM));
    if (err) {
      llvm::errs() << "addModule Error:" << err;
    }
  }

 private:
  LlvmJit(Mode mode, std::unique_ptr<llvm::orc::ExecutionSession> es, llvm::orc::JITTargetMachineBuilder jtmb, llvm::DataLayout dl) : mode(mode), data_layout(dl), execution_session(std::move(es)), object_layer(*execution_session, []() { return std::make_unique<llvm::SectionMemoryManager>(); }), compile_layer(*execution_session, object_layer, std::make_unique<llvm::orc::ConcurrentIRCompiler>(std::move(jtmb))), main_jit_dylib(execution_session->createBareJITDylib("<main>")) {
    if (this->mode == Mode::Verbose) {
      std::cerr << "LlvmJit() constructor" << std::endl;
    }
  }
  const Mode mode;

 public:
  // TODO: encapsulate somehow; why does codegen need to know this?
  const llvm::DataLayout data_layout;

 private:
  const std::unique_ptr<llvm::orc::ExecutionSession> execution_session;
  llvm::orc::RTDyldObjectLinkingLayer object_layer;
  llvm::orc::IRCompileLayer compile_layer;
  llvm::orc::JITDylib& main_jit_dylib;
};

}  // namespace torpul

#endif
