# Chapter 5: Control Flow

This chapter extends the parser and code generator to support conditional branching (`if/then/else` expressions) and loop constructs (`for/in` expressions).

## Concepts
* **Conditional Branching (`br`/`cond_br`)**: Emitting conditional branches based on comparison results.
* **Basic Block Layout**: Structuring blocks for then, else, loop, and merge.
* **Phi Nodes**: Selecting the correct value based on the incoming block execution path.

## LLVMSharp 5.0.0 Static APIs Used
* `LLVM.AppendBasicBlock`
* `LLVM.BuildCondBr`, `LLVM.BuildBr`
* `LLVM.PositionBuilderAtEnd`
* `LLVM.BuildPhi`
* `phiNode.AddIncoming`
