#ifndef LLVM_LOF_REDTREE_H
#define LLVM_LOF_REDTREE_H

#include "GreenTree.h"
#include <vector>

namespace llvm {
  class RedRoot;
  class RedSet;
  class RedReg;

	/// Node in an immutable tree, contains reference to parent and corresponding green node (which stores the children) 
	class RedNode {
  public:

    class const_child_iterator: public iterator_facade_base<
      const_child_iterator,
      std::forward_iterator_tag,
      const RedNode*>  {
    private:
    const  RedNode *parent;
      ArrayRef<const GreenNode*>::iterator git;
    public:
      const_child_iterator(const RedNode *parent,  ArrayRef<const GreenNode*>::iterator git) : parent(parent), git(git) {}

      const_child_iterator &operator=(const const_child_iterator &R) {
         this->parent= R.parent;
          this->git = R.git;
      }
      bool operator==(const const_child_iterator &R) const {
        assert(this->parent==R.parent);
      return this->git == R.git;
      }

      const RedNode* operator*() const {
        return parent->getChild(git);
      }

      RedNode* operator*() {
        return parent->getChild(git);
      }

      const_child_iterator &operator++() {
        git++;
        return *this;
      }
    };



	private:
		RedNode *Parent;
	const GreenNode *Green;
		
		// TODO: Combine into allocation of RedNote itself.
		mutable std::vector<RedNode *> Children;

    RedNode *getChild(ArrayRef<const GreenNode*>::iterator It) const { 
      return getChild(std::distance(Green->getChildren().begin(), It  ));
    }

	protected:
		RedNode(RedNode*Parent, const GreenNode *Green): Parent(Parent),Green(Green) {
      assert(Green);
      assert(!Parent == isa<GreenRoot>(Green));

			auto NChildren = Green->getChildren().size();
			Children.resize(NChildren);
			for (int i = 0; i<NChildren;i+=1)
				Children[i] = nullptr;
		}

	public:
		virtual ~RedNode() {};

		virtual LoopHierarchyKind getKind() const { return getGreen()->getKind();}
		static bool classof(const RedNode *) {	return true; }

		void dump() const { printText(errs()); }
		virtual void printLine(raw_ostream &OS) const { getGreen()->printLine(OS); }
		virtual void printText(raw_ostream &OS) const { getGreen()->printText(OS); }

		RedNode  *getParent() const {return Parent;}
	const	GreenNode*getGreen()  const {return Green;}


  iterator_range<const_child_iterator> children() const { 
    auto Children = getGreen()->getChildren();
    return make_range(
      const_child_iterator(this, Children.begin()), 
      const_child_iterator(this, Children.end())
    ); 
  }

  auto children_unexpanded() const -> llvm::iterator_range<decltype(Children.begin())>  {
    return make_range( Children.begin(), Children.end() );
  }

  // Does not implicitly expand red nodes
  // Possible alternative: PointerUnion<RedNode*,GreenNode*> which is a GreenNode if the RedNode is not expanded yet.
  auto children_redgreen() const -> decltype(zip( getGreen()->children(), make_range( Children.begin(), Children.end() )  )) {
	//  auto GreenRange = getGreen()->children();
	  auto RedRange = make_range( Children.begin(), Children.end() );
    return zip(getGreen()->children(), make_range( Children.begin(), Children.end() ));
  }


		int getNumChildren() const {  return Green->getChildren().size(); }
		RedNode *getChild(int i) const { 
				auto &Child = Children[i];
				if (!Child) {
					Child = Create(const_cast<RedNode*>( this), Green->getChildren()[i] );
				}
				return Child;
		}

    static RedNode *Create(RedNode*Parent,const GreenNode *Green);
	};






	

	class RedSequence final : public RedNode {
	private:
	public:
		RedSequence(RedNode*Parent, GreenSequence *Green) : RedNode(Parent,Green) {}
		virtual ~RedSequence() {};

		static bool classof(const RedNode *Node) { return GreenSequence::classof(Node->getGreen()); }
		static bool classof(const RedSequence *) {	return true;	}

    const 	GreenSequence* getGreen() const { return static_cast<const  GreenSequence*>( RedNode::getGreen());}
	};






	class RedRoot: public RedNode {
	private:
    bool AllDefsFound = false;

    static void assignDefinitions( RedNode *Node, DenseMap<Value*,RedSet*> &PastDefinitions);
  protected:
    RedRoot( GreenRoot *Green) : RedNode(nullptr, Green) {}
	public:

		virtual ~RedRoot() {};

		static bool classof(const RedNode *Node) { return GreenRoot::classof(Node->getGreen()); }
		static bool classof(const RedRoot *) { return true; }

    const GreenRoot* getGreen() const {return static_cast<const  GreenRoot*>( RedNode::getGreen());}



    static RedRoot *Create(GreenRoot* Green) { 
      return new RedRoot(Green);
    }


    void findAllDefinitions();

    bool hasDirectDependence(RedNode *Pred, RedNode *Succ);
	};







	
	class RedBlock : public RedNode {
	private:
	public:
		RedBlock(RedNode*Parent, GreenBlock *Green) : RedNode(Parent,Green) {}


		static bool classof(const RedNode *Node) { return GreenBlock::classof(Node->getGreen()); }
		static bool classof(const RedBlock *) {	return true;	}

    const 	GreenBlock* getGreen() const {return static_cast<const  GreenBlock*>( RedNode::getGreen());}
	};





	class RedLoop final : public RedBlock {
	private:
	public:
		RedLoop(RedNode *Parent, GreenLoop*Green) : RedBlock(Parent,Green) {}

		static bool classof(const RedNode *Node) {return  GreenLoop::classof(Node->getGreen()); }
		static bool classof(const RedLoop *) {	return true;	}

    const 	GreenLoop* getGreen() const {return static_cast<const GreenLoop*>(RedBlock:: getGreen());}
	};





	class RedStmt final : public RedBlock {
	private:
	public:
		RedStmt(RedNode *Parent, GreenStmt*Green) : RedBlock(Parent,Green) {}

		static bool classof(const RedNode *Node) {return  GreenStmt::classof(Node->getGreen()); }
		static bool classof(const RedStmt *) {	return true;	}

    const 	GreenStmt* getGreen() const {return static_cast<const GreenStmt*>(RedBlock:: getGreen());}
	};




	class RedInst  : public RedNode {
	private:
	public:
		RedInst(RedNode *Parent, GreenInst*Green) : RedNode(Parent,Green) {}

		static bool classof(const RedNode *Node) {return  GreenInst::classof(Node->getGreen()); }
		static bool classof(const RedInst *) {	return true;	}

    const 	GreenInst* getGreen() const {return static_cast<const GreenInst*>(RedNode:: getGreen());}
	};






	class RedStore final : public RedInst {
	private:
  protected:
    RedStore(RedNode *Parent, GreenStore*Green) : RedInst(Parent,Green) {}

	public:
		static bool classof(const RedNode *Node) {return  GreenStore::classof(Node->getGreen()); }
		static bool classof(const RedStore *) {	return true;	}

    const 	GreenStore* getGreen() const {return static_cast<const GreenStore*>( RedInst:: getGreen());}

    static   RedStore *Create(RedNode*Parent, GreenStore *Green) {
      auto Result = new RedStore(Parent,Green);
      return Result;
    }
	};







	class RedSet final : public RedInst {
	private:
  protected:
    RedSet(RedNode *Parent, GreenSet*Green) : RedInst(Parent,Green) {}

	public:
		static bool classof(const RedNode *Node) {return  GreenSet::classof(Node->getGreen()); }
		static bool classof(const RedSet *) {	return true;	}

    const 	GreenSet* getGreen() const {return static_cast<const GreenSet*>(RedInst:: getGreen());}

    Instruction *getVar( ) const {return  getGreen()->getVar(); }


    static   RedSet *Create(RedNode*Parent, GreenSet *Green) {
      auto Result = new RedSet(Parent,Green);
      return Result;
    }
	};








	class RedExpr : public RedNode {
	private: 
	public:
		RedExpr(RedNode *Parent,const  GreenExpr*Green) : RedNode(Parent,Green) {}

		static bool classof(const RedNode *Node) {return  GreenExpr::classof(Node->getGreen()); }
		static bool classof(const RedExpr *) {	return true;	}

    const 	GreenExpr* getGreen() const {return static_cast<const GreenExpr*>(RedNode:: getGreen());}
	};





	class RedConst final : public RedExpr {
	private:
	public:
		RedConst(RedNode *Parent, GreenConst*Green) : RedExpr(Parent,Green) {}

		static bool classof(const RedNode *Node) {return  GreenConst::classof(Node->getGreen()); }
		static bool classof(const RedConst *) {	return true;	}

    const 	GreenConst* getGreen() const {return static_cast<const GreenConst*>(RedExpr:: getGreen());}
	};



  class RedArg final : public RedExpr {
  private:
  public:
    RedArg(RedNode *Parent, GreenArg*Green) : RedExpr(Parent,Green) {}

    static bool classof(const RedNode *Node) {return  GreenConst::classof(Node->getGreen()); }
    static bool classof(const RedArg *) {	return true;	}

    const GreenArg* getGreen() const {return static_cast<const GreenArg*>(RedExpr:: getGreen());}
  };





	class RedReg final : public RedExpr {
    friend class RedRoot;
	private:
    // Where the definition of the register is in this RedTree.
     RedSet *Def;

  protected:
    RedReg(RedNode *Parent,const  GreenReg*Green) : RedExpr(Parent,Green) {}

	public:
		static bool classof(const RedNode *Node) { return GreenReg::classof(Node->getGreen()); }
		static bool classof(const RedConst *) {	return true;	}

    const 	GreenReg* getGreen() const {return static_cast<const GreenReg*>(RedExpr:: getGreen());}
    Value *getVar() const { return getGreen()->getVar(); }

    RedSet *getDef() const {
      return Def;
    }


 static RedReg *Create(RedNode*Parent,const  GreenReg *Green) {
      auto Result = new RedReg(Parent, Green);
      return Result;
    }
	};









	class RedGEP final : public RedExpr {
	private:
	public:
		RedGEP(RedNode *Parent, GreenReg*Green) : RedExpr(Parent,Green) {}

		static bool classof(const RedReg *Node) { return GreenGEP::classof(Node->getGreen()); }
		static bool classof(const RedConst *) {	return true;	}

    const 		GreenGEP* getGreen() const {return static_cast<const GreenGEP*>(RedExpr:: getGreen());}
	};





	class RedICmp final : public RedExpr {
	private:
	public:
		RedICmp(RedNode *Parent, const GreenReg*Green) : RedExpr(Parent,Green) {}

		static bool classof(const RedReg *Node) { return GreenICmp::classof(Node->getGreen()); }
		static bool classof(const RedConst *) {	return true;	}

    const 	GreenICmp* getGreen() const {return static_cast<const GreenICmp*>(RedExpr:: getGreen());}
	};
}

#endif /* LLVM_LOF_REDTREE_H */

