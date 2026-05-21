# Chapter 4: JIT & Optimizations

This chapter adds function-level optimization passes and configures a JIT Execution Engine (MCJIT or ORC JIT) to compile and execute AST expressions interactively in the REPL.

## Concepts
* **Pass Manager**: Orchestrates compilation optimization passes (e.g. instruction combining, reassociation, common subexpression elimination).
* **Execution Engine / MCJIT**: Compiles LLVM IR modules to machine code on the fly and retrieves callable native function pointers.
* **REPL Evaluation**: Evaluates top-level expressions immediately, executing them and printing the result.

## LLVMSharp 20.1.2 Static APIs Used
* `LLVM.CreateFunctionPassManagerForModule`
* `LLVM.AddInstructionCombiningPass`, `LLVM.AddReassociatePass`, `LLVM.AddGVNPass`, `LLVM.AddCFGSimplificationPass`
* `LLVM.InitializeFunctionPassManager`
* `LLVM.RunFunctionPassManager`
* `LLVM.LinkInMCJIT`, `LLVM.InitializeNativeTarget`, `LLVM.InitializeNativeAsmPrinter`
* `LLVM.CreateMCJITCompilerForModule`
* `LLVM.GetFunctionAddress`
