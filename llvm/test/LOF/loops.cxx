#include "compiledtestboilerplate.h"
#include <llvm/LOF/LoopOpt.h>
#include <llvm/LOF/Green.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/AsmParser/Parser.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/IR/DiagnosticPrinter.h>
#include <llvm/LOF/LoopOptPass.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/Dominators.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include "gtest/gtest.h"
using namespace llvm;
using namespace lof;





static std::unique_ptr<Module> parseAssembly(const char *Assembly, LLVMContext &Context) {
  llvm::SMDiagnostic Error;
  std::unique_ptr<Module> M = parseAssemblyString(Assembly, Error, Context);

  if (!M) {
    std::string ErrMsg;
    raw_string_ostream OS(ErrMsg);
    Error.print("", OS);
    report_fatal_error(OS.str());
  }

  assert(M && !verifyModule(*M, &errs()));
  return M;
}









static const char* loop_reduction = R"IR(
define i32 @loopfunc() {
entry:
  br label %for.header

for.header:
  %sum = phi i32 [ 0, %entry ], [ %add, %for.latch ]
  %cond = icmp slt i32 %sum, 32
  br i1 %cond, label %for.body, label %for.exit

for.body:
  %add = add i32 %sum, 1
  br label %for.latch

for.latch:
  br label %for.header

for.exit:
  ret i32 %sum ; force a use outside the loop
}
)IR";



TEST(BasicTest, Reduction) {
  LLVMContext Context;
  auto M = parseAssembly(loop_reduction, Context);

  std::vector<std::unique_ptr<lof::LoopOptimizer>> LOFs;
  auto MPM = new llvm::legacy::PassManager();
  MPM->add(createLoopFrameworkAnalyzerPass(LOFs));
  MPM->run(*M.get());

  for (auto &LOF : LOFs) {
    auto Root = LOF->getOriginalRoot();

    Root->dump();
    auto Normalized = LOF->normalize(Root);
    Normalized->dump();
  }
}








static const char* loop_nonrotated_constantbounds = R"IR(
declare void @body(i32)

define void @loopfunc() {
entry:
  br label %for.header

for.header:
  %i = phi i32 [ 0, %entry ], [ %i.next, %for.latch ]
  %cond = icmp slt i32 %i, 32
  br i1 %cond, label %for.body, label %for.exit

for.body:
  call void @body(i32 %i)
  br label %for.latch

for.latch:
  %i.next = add nsw i32 %i, 1
  br label %for.header

for.exit:
  ret void
}
)IR";




TEST(BasicTest, LoopNonrotatedConstantBounds) {
  return;
  LLVMContext Context;
  auto M = parseAssembly(loop_nonrotated_constantbounds, Context);

  std::vector<std::unique_ptr<lof::LoopOptimizer>> LOFs;

  auto MPM = new llvm::legacy::PassManager();
  MPM->add(createLoopFrameworkAnalyzerPass(LOFs));
  MPM->run(*M.get());

  for (auto &LOF : LOFs) {
    auto Root = LOF->getOriginalRoot();
    Root->dump();
  }

  ASSERT_EQ(LOFs.size(),1);
  auto LoopLOF = LOFs[0].get();

  auto Root = LoopLOF->getOriginalRoot()->findNode("loopfunc");
  ASSERT_FALSE(Root->isLoop());
  ASSERT_EQ(Root->getNumChildren(),1);

  auto Loop = Root->findNode("for.header", false);
  ASSERT_TRUE(  Loop->isLoop() );
  ASSERT_EQ(Loop->getNumChildren(),2);

  auto Recurrence = Loop->getChild(0);
  ASSERT_EQ(cast<lof::Green>(Recurrence)->getName(), "i");
  ASSERT_EQ(cast<lof::Green>(Recurrence)->getOperation().getKind(), Operation::Select );

  auto Body = Loop->getChild(1);
  ASSERT_EQ(cast<lof::Green>(Body)->getOperation().getKind(), Operation::LLVMInst );
}
