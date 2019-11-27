set(LLVM_LINK_COMPONENTS
  AllTargetsAsmParsers
  AllTargetsCodeGens
  AllTargetsDescs
  AllTargetsInfos
  AggressiveInstCombine
  Analysis
  BitWriter
  CodeGen
  Core
  Coroutines
  IPO
  IRReader
  InstCombine
  Instrumentation
  MC
  ObjCARCOpts
  Remarks
  ScalarOpts
  Support
  Target
  TransformUtils
  Vectorize
  Passes
  LOF
  )

# Support plugins.
set(LLVM_NO_DEAD_STRIP 1)

add_llvm_library(regression
  AnalysisWrappers.cpp
  BreakpointPrinter.cpp
  Debugify.cpp
  GraphPrinters.cpp
  NewPMDriver.cpp
  PassPrinters.cpp
  PrintSCC.cpp
  regression.cpp

  DEPENDS
  intrinsics_gen
  )
#export_executable_symbols(regression)

if(WITH_POLLY AND LINK_POLLY_INTO_TOOLS)
  target_link_libraries(regression PUBLIC Polly)
endif(WITH_POLLY AND LINK_POLLY_INTO_TOOLS)
