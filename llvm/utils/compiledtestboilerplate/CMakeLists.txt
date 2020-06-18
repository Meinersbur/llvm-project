set(LLVM_LINK_COMPONENTS
  AllTargetsAsmParsers
  AllTargetsCodeGens
  AllTargetsDescs
  AllTargetsInfos
  AggressiveInstCombine
  Analysis
  AsmParser
  BitWriter
  CodeGen
  Core
  Coroutines
  Extensions
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
  
# Our current version of gtest does not properly recognize C++11 support
# with MSVC, so it falls back to tr1 / experimental classes.  Since LLVM
# itself requires C++11, we can safely force it on unconditionally so that
# we don't have to fight with the buggy gtest check.
add_definitions(-DGTEST_LANG_CXX11=1)
add_definitions(-DGTEST_HAS_TR1_TUPLE=0)

include_directories(${LLVM_MAIN_SRC_DIR}/utils/unittest/googletest/include)
include_directories(${LLVM_MAIN_SRC_DIR}/utils/unittest/googlemock/include)
if (NOT LLVM_ENABLE_THREADS)
  list(APPEND LLVM_COMPILE_DEFINITIONS GTEST_HAS_PTHREAD=0)
endif ()


add_llvm_library(compiledtestboilerplate
  AnalysisWrappers.cpp
  BreakpointPrinter.cpp
  GraphPrinters.cpp
  NewPMDriver.cpp
  PassPrinters.cpp
  PrintSCC.cpp
  compiledtestboilerplate.cpp

  DEPENDS
  intrinsics_gen
  #SUPPORT_PLUGINS
  )
export_executable_symbols_for_plugins(compiledtestboilerplate)

if(LLVM_BUILD_EXAMPLES)
    target_link_libraries(compiledtestboilerplate PRIVATE ExampleIRTransforms)
endif(LLVM_BUILD_EXAMPLES)

target_include_directories(compiledtestboilerplate INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)
#target_link_libraries(compiledtestboilerplate PUBLIC gtest_main gtest ${LLVM_PTHREAD_LIB})
