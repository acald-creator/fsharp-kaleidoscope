# Chapter 7: Mutable Variables

This chapter introduces mutable local variables and variable assignment. It moves the compiler into using local stack allocations (`alloca`) instead of direct AST-value bindings, relying on LLVM's `mem2reg` pass to promote these allocations back to SSA form.

## Concepts
* **Stack Allocations (`alloca`)**: Allocating space on the stack at function entry for mutable variables.
* **Loads & Stores**: Reading from stack spaces using `load` and updating them using `store`.
* **SSA Promotion (`mem2reg`)**: Converting the load/store pattern into optimized Static Single Assignment registers automatically.

## LLVMSharp 5.0.0 Static APIs Used
* `LLVM.BuildAlloca`
* `LLVM.BuildLoad`
* `LLVM.BuildStore`
* `LLVM.AddPromoteMemoryToRegisterPass`
