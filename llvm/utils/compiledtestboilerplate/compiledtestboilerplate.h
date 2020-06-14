#include "gtest/gtest.h"
#include <memory>
#include <llvm/ADT/ArrayRef.h>

namespace llvm {
  class Module;
  class LLVMContext;
}



std::unique_ptr<llvm::Module> run_opt(const char* FileName, const char* Def, llvm::ArrayRef<const char *> Args);


