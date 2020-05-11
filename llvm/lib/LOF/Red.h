#ifndef LLVM_LOF_RED_H
#define LLVM_LOF_RED_H

#include "Green.h"

namespace lof {
  class Red;

  class  red_child_iterator :public map_index_iterator<red_child_iterator, Red*, Red*, std::ptrdiff_t, Red**, Red* > {
  public:
    red_child_iterator(Red* Parent, size_t Idx) : map_index_iterator(Parent, Idx) {}

  public:
    Red* operator*() const;
  }; // class red_child_iterator


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
    static Red* createRoot(GCommon* Root) {
        return new Red(Root, nullptr, -1);
      }


    public:
      bool isRoot() const { return Parent == nullptr; }
      Red* getParent() const { return Parent; }
      size_t getNumChildren() const { return G->getNumChildren(); }
      GCommon* getGreen() const { return G; }

    private:
      // TODO: use llvm::TrailingObjects
      std::unique_ptr<Red*[]> Children;
    public:
      Red* getChild(size_t i);
      ArrayRef<Red*> getChildren();
      auto children() { return make_range( red_child_iterator(this,0), red_child_iterator(this,getNumChildren()) ); }
      auto childen_ref() { return llvm::make_pointee_range(children()); }



    public:
      bool isContainer() const { return G->isContainer(); }
      bool isInstruction() const { return G->isInstruction(); }
      bool isExpr() const { return G->isExpr(); }

      bool isStmt() const { return G->isStmt(); }
      bool isLoop() const { return G->isLoop(); }

    }; // class Red

  inline  Red * red_child_iterator::operator*() const {
      return Container->getChild(Idx);
    }

} // namespace lof



#endif /* LLVM_LOF_RED_H */
