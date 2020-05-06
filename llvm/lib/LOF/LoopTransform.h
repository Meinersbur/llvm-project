#ifndef LLVM_LOF_LOOPTRANSFORM_H
#define LLVM_LOF_LOOPTRANSFORM_H

#include "Green.h"
#include "LoopContext.h"

namespace lof {
  
    // Apply Unroll-And-Jam.
    // G must be a loop node.
    //
    // This always jams the innermost loop(s), so no jam depth needs to be provided. 
    Green* applyUnrollAndJam(LoopContext &LoopCtx, Green* G, int Factor);


} // namespace lof
#endif /* LLVM_LOF_LOOPTRANSFORM_H */
