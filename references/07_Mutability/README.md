# Chapter 7: Mutable Variables

This chapter introduces mutable local variables and variable assignment. It moves the compiler into using local stack allocations (`alloca`) instead of direct AST-value bindings, relying on LLVM's `mem2reg` pass to promote these allocations back to SSA form.

## Concepts
* **Stack Allocations (`alloca`)**: Allocating space on the stack at function entry for mutable variables.
* **Loads & Stores**: Reading from stack spaces using `load` and updating them using `store`.
* **SSA Promotion (`mem2reg`)**: Converting the load/store pattern into optimized Static Single Assignment registers automatically.

## LLVMSharp 20.1.2 Static APIs Used
* `LLVM.BuildAlloca` (requires type parameter due to opaque pointers)
* `LLVM.BuildLoad2` (replacing deprecated `LLVM.BuildLoad`, requiring type parameter)
* `LLVM.BuildStore`
* `LLVM.AddPromoteMemoryToRegisterPass`
