# Chapter 3: Code Generation

This chapter maps the AST nodes defined in Chapter 2 to LLVM IR using the LLVMSharp 5.0.0 bindings.

## Concepts
* **LLVM Context**: Manages global LLVM states, types, and constants.
* **LLVM Module**: A container for functions, global variables, and symbol tables.
* **LLVM Builder**: The helper object used to generate LLVM instructions.
* **Symbol Table**: Keeps track of defined variables and parameter references during codegen.

## LLVMSharp 5.0.0 Static APIs Used
* `LLVM.ModuleCreateWithNameInContext`
* `LLVM.CreateBuilderInContext`
* `LLVM.BuildFAdd`, `LLVM.BuildFSub`, `LLVM.BuildFMul`
* `LLVM.BuildCall`
* `LLVM.AddFunction`, `LLVM.AppendBasicBlock`
