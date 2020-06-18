#include "llvm/ADT/ArrayRef.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/ADT/iterator.h"
#include "llvm/IR/Instructions.h"
#include "gtest/gtest.h"
#include <memory>



template<typename Ty>
class iterator_range_set : public llvm::iterator_range<Ty> {
public:
  iterator_range_set(Ty x, Ty y) :llvm:: iterator_range<Ty>(std::move(x), std::move(y)) {}


 bool all_of(std::function<bool(typename std::iterator_traits<Ty>::reference)> Pred, int AssertMinMatches = 0) const {
   expectMinCount(AssertMinMatches);
   return llvm::all_of(*this, Pred);
 }

 bool any_of(std::function<bool(typename std::iterator_traits<Ty>::reference)> Pred) const {
   return llvm::any_of(*this, Pred);
 }

 auto count() const {
   return count_if(*this, [](const auto &) {return true; });
 }

 // TODO: This should generally not require another complete iteration over everything
 void expectMinCount(int AssertMinMatches) const {
   if (AssertMinMatches <= 0)
     return;
   EXPECT_GE(count(), AssertMinMatches);
 }
};


template <class T> iterator_range_set<T> make_range_set(T x, T y) {
  return iterator_range_set<T>(std::move(x), std::move(y));
}


template<typename Ty>
class inst_iterator_range_set : public iterator_range_set<Ty> {
  //static_assert(std::is_same<typename std::iterator_traits<Ty>::value_type , llvm::Instruction*>::value, "Use iterator_range_set for iterators over something else than llvm::Instruction*");
public:
  inst_iterator_range_set(Ty x, Ty y): iterator_range_set<Ty>(std::move(x), std::move(y)) {}

  class range_op_iterator : public llvm::iterator_facade_base<range_op_iterator, std::forward_iterator_tag, llvm::Use> {
  private:
    llvm::User::op_iterator OpIter;
    llvm::Optional<Ty> InstIter;
    llvm::Optional<Ty> InstEnd;

  public:
    range_op_iterator(llvm::User::op_iterator OpIter, Ty  InstIter, Ty InstEnd) : OpIter(std::move(OpIter)),InstIter(std::move(InstIter)),  InstEnd(std::move(InstEnd)) { }
    range_op_iterator() : OpIter{}, InstIter{llvm::None}, InstEnd{llvm::None} {}

    range_op_iterator(const range_op_iterator& R) = default;
    range_op_iterator& operator=(const range_op_iterator& R) {
      this->OpIter = R.OpIter;
      this->InstIter = R.InstIter;
      this->InstEnd = R.InstEnd;
      return *this;
    }
    bool operator==(const range_op_iterator& R) const {
      return this->OpIter == R.OpIter;
    }
    llvm::Use& operator*() const {
      return *OpIter;
    }


    range_op_iterator& operator++() {
      ++OpIter;

    retry_op:
    llvm::  Instruction* I = *InstIter.getValue();
      if (OpIter != I->op_end())
        return *this;

    retry_inst:
      ++InstIter.getValue();
      if (InstIter != InstEnd) {
        OpIter = I->op_begin();
        goto retry_op;
      }

      OpIter = llvm::User::op_iterator{};
      return *this;
    }
  };

  auto operands() {
    auto FirstInst = this->begin();
    auto FirstOp = (*FirstInst)->op_begin();
    auto EndInst = this->end();
    auto B = range_op_iterator(FirstOp, FirstInst, EndInst);
    auto E = range_op_iterator{};
    return make_range_set(B, E);
  }
};


template <class T>
inst_iterator_range_set<T> make_inst_iterator_range_set(T x, T y) {
  return inst_iterator_range_set<T>(std::move(x), std::move(y));
}

template <typename R>
auto distance(R &&Range) {
  return std::distance(Range.begin(), Range.end());
}

struct FunctionResult {
  llvm::Function* F;

  FunctionResult(llvm::Function* F) : F(F) {  }

  class all_insts_iterator : public llvm::iterator_facade_base<all_insts_iterator, std::forward_iterator_tag, llvm::Instruction*, ptrdiff_t, llvm::Instruction* *, llvm::Instruction*> {
  private:
    llvm::BasicBlock::iterator InstIter;

  public:
    all_insts_iterator(llvm::BasicBlock::iterator InstIter): InstIter(std::move(InstIter)) {}
    all_insts_iterator() {}

    all_insts_iterator(const all_insts_iterator &R) = default;
    all_insts_iterator &operator=(const all_insts_iterator &R) {
      this->InstIter = R.InstIter;
      return *this;
    }
    bool operator==(const all_insts_iterator &R) const {
      return this->InstIter == R.InstIter;
    }
    llvm::Instruction *operator*() const {
      return &*InstIter;
    }

    all_insts_iterator &operator++() {
      auto  P = InstIter->getParent();
      ++InstIter;
      if (InstIter == P->end()) {
        auto NextBB = P->getNextNode();
        if (NextBB) {
          InstIter = NextBB->begin();
        } else {
          InstIter = llvm::BasicBlock::iterator{};
        }
      }
      return *this;
    }
  };


  auto insts() {
    auto FirstBB = F->begin();
    auto FirstInst = FirstBB->begin();
    return make_inst_iterator_range_set( all_insts_iterator{ FirstInst }, all_insts_iterator{} );
  }

  auto insts(std::function<bool(llvm::Instruction*)> Pred) {
   auto  Range = llvm::make_filter_range(insts(), Pred);
   return make_inst_iterator_range_set(Range.begin(), Range.end() );
  }

  auto call_insts() {
    auto Filter =  llvm::make_filter_range(insts(),  [](llvm::Instruction*I) -> bool { return llvm:: isa<llvm::CallInst>(I); });
    auto Map = llvm::map_range(Filter, [](llvm::Instruction* I) -> auto { return llvm:: cast<llvm::CallInst>(I); });
    return make_inst_iterator_range_set( Map.begin(), Map.end() );
  }

  auto call_insts(std::function<bool(llvm::CallInst*)> Pred, int ExpectAtLeast = 0) {
    auto Filter = llvm::make_filter_range(call_insts(), Pred);
    auto Result = make_inst_iterator_range_set( Filter.begin(), Filter.end() );
    Result.expectMinCount(ExpectAtLeast);
    return Result;
  }


  class all_op_iterator : public llvm::iterator_facade_base<all_op_iterator, std::forward_iterator_tag, llvm::Use> {
  private:
    llvm::User::op_iterator OpIter;

  public:
    all_op_iterator(llvm::User::op_iterator OpIter): OpIter(std::move(OpIter)) {      }
    all_op_iterator() : OpIter{}  {}

    all_op_iterator(const all_op_iterator &R) = default;
    all_op_iterator &operator=(const all_op_iterator &R) {
      this->OpIter = R.OpIter;
      return *this;
    }
    bool operator==(const all_op_iterator &R) const {
      return this->OpIter == R.OpIter;
    }
    llvm::Use &operator*() const {
      return *OpIter;
    }



    all_op_iterator &operator++() {
      auto I = llvm::cast<llvm::Instruction>(OpIter->getUser());
      auto BB = I->getParent();

      ++OpIter;

retry_op:
      if (OpIter != I->op_end())
         return *this;

retry_inst:
      I = I->getNextNode();
      if (I) {
        OpIter = I->op_begin();
        goto retry_op;
      }

      BB = BB->getNextNode();
      if (BB) {
        I = &BB->front();
        goto retry_inst;
      }

      OpIter =  llvm::User::op_iterator{};
      return *this;
    }
  };



  auto operands() {
    auto FirstBB = F->begin();
    auto FirstInst = FirstBB->begin();
    // TODO: Instruction without operands?
    auto FirstOp = FirstInst->operands().begin();
    return make_range_set(all_op_iterator{ FirstOp }, all_op_iterator{});
  }

  auto operands(std::function<bool(const llvm::Use &)> Pred, int AssertMinCount=0) {
    auto Ops = operands();
    auto Filter = llvm::make_filter_range(Ops, Pred);
   auto Result = make_range_set(Filter.begin(), Filter.end());
   Result.expectMinCount(AssertMinCount);
   return Result;
  }
};


struct ModuleResult {
   llvm::LLVMContext Context;
   std::unique_ptr<llvm::Module> M;

   FunctionResult getFunc(llvm::StringRef Name) {
     return {M->getFunction(Name)};
   }
};

std::unique_ptr<ModuleResult> run_opt(llvm::StringRef FileName, llvm::StringRef Def, llvm::ArrayRef<const char*> Args);
