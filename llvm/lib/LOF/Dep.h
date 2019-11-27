#ifndef LLVM_LOF_DEP_H
#define LLVM_LOF_DEP_H

namespace llvm {

  enum class DepKind {
    Unknown = 0,

    Mem = 0b01,   // Load/Store dependency
    Reg = 0b10,   // SSA dominance condition
    Ctrl = 0b11,  // SSA dominance condition for atoms

    Flow = 0b01 << 0b100,  // Read-after-write
    Anti = 0b10 << 0b100,  // Write-after-read (To avoid a value-still-live being overwritten)
    Output = 0b10 << 0b100,  // Write-after-write (to avoid a write to being moved into a flow dependence)

    MemFlow = Mem | Flow,
    MemAnti = Mem | Anti,
    MemOutput = Mem | Output,

    RegFlow = Reg | Flow,
    RegAnti = Reg | Anti,
    RegOutput = Reg | Output,

    CtrlFlow = Reg | Flow,
    CtrlAnti = Reg | Anti,
    CtrlOutput = Reg | Output,
  };

}

#endif /* LLVM_LOF_DEP_H */
