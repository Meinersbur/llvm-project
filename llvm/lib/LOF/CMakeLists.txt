add_llvm_library(LLVMLOF
  Dep.cpp
  LoopOpt.cpp
  LoopOptPass.cpp
  Green.cpp
  GreenBuilder.cpp
  Red.cpp
  LoopTreeBuilder.cpp
  LoopTreeCodegen.cpp
  LOFUtils.cpp

  ADDITIONAL_HEADER_DIRS
    ${LLVM_MAIN_INCLUDE_DIR}/llvm/LOF
    
  DEPENDS
  intrinsics_gen
)
