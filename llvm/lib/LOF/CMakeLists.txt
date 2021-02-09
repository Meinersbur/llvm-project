add_llvm_component_library(
    LLVMLOF Dep.cpp LoopOpt.cpp LoopOptPass.cpp Green.cpp GreenBuilder.cpp Red
        .cpp RedRef.cpp LoopTreeConverter.cpp LoopTreeCodegen.cpp LOFUtils
        .cpp ISLDep.cpp SimpleDep.cpp Affine.cpp LoopTransform.cpp LoopContext
        .cpp LoopTreeTransform.cpp

            ADDITIONAL_HEADER_DIRS ${LLVM_MAIN_INCLUDE_DIR} /
    llvm /
    LOF

        DEPENDS intrinsics_gen)
