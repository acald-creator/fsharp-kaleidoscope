# Chapter 6: User-Defined Operators

This chapter extends the lexer, parser, and code generator to allow users to declare custom prefix unary and infix binary operators, complete with custom precedence.

## Concepts
* **Dynamic Precedence Table**: Updating the precedence parsing lookup maps at parse time.
* **Unary and Binary Function Declarations**: Translating operator names to standard function call structures (e.g. `unary!` or `binary+`).

## LLVMSharp 5.0.0 Static APIs Used
* Generates normal `LLVM.AddFunction` and `LLVM.BuildCall` targets.
