add_llvm_library(LLVMLOF
  GreenTree.cpp
  RedTree.cpp
  Tree.cpp
  Dep.cpp
  LoopOpt.cpp
  LoopOptPass.cpp

  ADDITIONAL_HEADER_DIRS
    ${LLVM_MAIN_INCLUDE_DIR}/llvm/LOF
    
  DEPENDS
  intrinsics_gen
)
