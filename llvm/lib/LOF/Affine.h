#ifndef LLVM_LOF_AFFINE_H
#define LLVM_LOF_AFFINE_H

// Only forward declarations
struct isl_set;

namespace llvm {
namespace lof {

class AffineSet {
private:
  isl_set *Set;
  isl_set *Space;

public:
}; // class AffineSet

} // namespace lof
} // namespace llvm

#endif /* LLVM_LOF_AFFINE_H */
