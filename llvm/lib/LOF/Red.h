#ifndef LLVM_LOF_RED_H
#define LLVM_LOF_RED_H

#include "Green.h"

namespace lof {
    class Red {
    private:
      GCommon* G;
      Red* Parent;
      int ParentIdx;

    private:
      Red(GCommon* G, Red* Parent, int ParentIdx) : G(G), Parent(Parent) {
        assert(G);
        assert(!G->isStaging());
        if (Parent) {
          assert(ParentIdx >= 0);
          assert(ParentIdx < Parent->getNumChildren());
          // TODO: Check if G is one of Parent's nodes
        }
      }
    public :
    static   Red* createRoot(GCommon* Root) {
        return new Red(Root, nullptr, -1);
      }


    public:
      bool isRoot() const { return Parent == nullptr; }
      Red* getParent() const { return Parent; }
      size_t getNumChildren() const { return G->getNumChildren(); }
      GCommon* getGreen() const { return G; }

    private:
      std::unique_ptr<Red*[]> Children;
    public:
      ArrayRef<Red*> getChildren();

    }; // class Red


} // namespace lof
#endif /* LLVM_LOF_RED_H */
