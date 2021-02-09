#ifndef LLVM_LOF_DEP_H
#define LLVM_LOF_DEP_H

#include "RedRef.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/LOF/Green.h"
#include <limits>

namespace lof {

struct SymAccessRef {
  enum AccessKind { NotApplicable, Def, Use };

  RedRef Stmt;
  GSymbol *Sym;
  AccessKind Kind;
};

SymAccessRef prevSymAccess(const RedRef &StartStmt, GSymbol *Sym,
                           SymAccessRef::AccessKind Kind, bool FindDefs,
                           bool FindUses);

SymAccessRef nextSymAccess(const RedRef &StartStmt, GSymbol *Sym,
                           SymAccessRef::AccessKind Kind);

void computeReachableDefs(Red *Root);

enum class DepKind {
  Unknown = 0,

  Mem = 0b01,  // Load/Store dependency
  Reg = 0b10,  // SSA dominance condition
  Ctrl = 0b11, // SSA dominance condition for atoms

  Flow = 0b01 << 0b100,   // Read-after-write
  Anti = 0b10 << 0b100,   // Write-after-read (To avoid a value-still-live being
                          // overwritten)
  Output = 0b10 << 0b100, // Write-after-write (to avoid a write to being moved
                          // into a flow dependence)

  MemFlow = Mem | Flow,
  MemAnti = Mem | Anti,
  MemOutput = Mem | Output,

  RegFlow = Reg | Flow,
  RegAnti = Reg | Anti,
  RegOutput = Reg | Output,

  CtrlFlow = Reg | Flow,
  CtrlAnti = Reg | Anti,
  CtrlOutput = Reg | Output,
}; // enum class DepKind

class Dep {
public:
  enum Kind {
    Unknown,

    JohnnyDep,

    LastKind = JohnnyDep
  };

  virtual Kind getKind() const = 0;
  static bool classof(const Dep *) { return true; }

public:
  virtual ~Dep() {}

  virtual bool isScalar() const = 0;
}; // class Dep

Green *detectArrays(LoopContext &Ctx, Green *Root);
Green *detectReductions(LoopContext &Ctx, Green *Root);

std::vector<Dep *> getAllDependencies(Green *Root);
bool checkDependencies(Green *NewRoot, ArrayRef<Dep *> Deps);

} // namespace lof
#endif /* LLVM_LOF_DEP_H */
