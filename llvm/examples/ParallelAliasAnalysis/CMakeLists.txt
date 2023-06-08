set(LLVM_LINK_COMPONENTS Core Support)
add_llvm_pass_plugin(ParallelAliasAnalysis
    ParallelAliasAnalysis.cpp
  DEPENDS
    intrinsics_gen
    BUILDTREE_ONLY
   )
set_target_properties(${name} PROPERTIES FOLDER "Examples")
