module Codegen

open LLVMSharp
open AST
open System.Collections.Generic

let context = LLVM.GetGlobalContext()
let themodule = LLVM.ModuleCreateWithNameInContext("module", context)
let builder = LLVM.CreateBuilderInContext(context)

let namevalues = Dictionary<string, LLVMValueRef>()

let rec codegen expr =
    let (!) = codegen
    let (!!) = List.map codegen >> List.toArray

    match expr with
    | Expr.Number f -> LLVM.ConstReal(LLVM.DoubleTypeInContext(context), f)
    | Expr.Variable n -> namevalues.[n]
    | Expr.Binop((op, _), lhs, rhs) ->
        match op with
        | "+" -> LLVM.BuildFAdd(builder, !lhs, !rhs, "addtmp")
        | "-" -> LLVM.BuildFSub(builder, !lhs, !rhs, "subtmp")
        | "*" -> LLVM.BuildFMul(builder, !lhs, !rhs, "multmp") 
        | "<" ->
            let boolVal = LLVM.BuildFCmp(builder, LLVMRealPredicate.LLVMRealULT, !lhs, !rhs, "cmptmp") 
            LLVM.BuildUIToFP(builder, boolVal, LLVM.DoubleTypeInContext(context), "booltmp")
        | _ -> failwithf "Invalid Operator %s" op
    | Expr.Call(callee, args) -> LLVM.BuildCall(builder, !callee, !! args, "calltmp")
    | Expr.Func(name, param, body) ->
        let func =
            let typ = [| for _ in param -> LLVM.DoubleTypeInContext(context) |]
            let ty = LLVM.FunctionType(LLVM.DoubleTypeInContext(context), typ, false) 
            LLVM.AddFunction(themodule, name, ty)
        namevalues.[name] <- func
        for i, p in List.indexed param do 
            namevalues.[p] <- func.GetParam(uint32 i)
        let bb = LLVM.AppendBasicBlock(func, "entry")
        LLVM.PositionBuilderAtEnd(builder, bb) 
        let _ = LLVM.BuildRet(builder, !body)
        func
    | Expr.Extern(name, param) ->
        let func =
            let typ = [| for _ in param -> LLVM.DoubleTypeInContext(context) |]
            let ty = LLVM.FunctionType(LLVM.DoubleTypeInContext(context), typ, false) 
            LLVM.AddFunction(themodule, name, ty)
        namevalues.[name] <- func
        func

let dump_ir expr =
    let value = codegen expr
    value.Dump()
    printfn ""
