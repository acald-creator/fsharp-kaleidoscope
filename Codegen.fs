module Codegen

open LLVMSharp
open Parser
open System.Collections.Generic

let context = LLVMContextRef.Global
let themodule = context.CreateModuleWithName "module"
let builder = context.CreateBuilder()

let namevalues = Dictionary<_, _>()

let rec codegen expr =
    let (!) = codegen
    let (!!) = List.map codegen >> List.toArray

    match expr with
        | Expr.Number f -> LLVMValueRef.CreateConstReal(context.DoubleType, f)
        | Variable n -> name_values.[n]
        | Binop((op, _), rhs, lhs) ->
            match op with
                | "+" -> builder.BuilderFAdd(!lhs, !rhs)
                | "-" -> builder.BuilderFSub(!lhs, !rhs)
                | "*" -> builder.BuilderFMul(!lhs, !rhs) 
                | "<" ->
                    let bool = builder.BuilderFCmp(LLVMRealPredicate.LLVMRealULT, !lhs, !rhs) 
                    builder.BuilderUIToFP(bool, context.DoubleType)
                | _ -> failwithf "Invalid Operator %)" op
            | Call(callee, args) -> builder.BuildCall(!callee, !! args)
            | Func(name, parem, body) ->
                let func =
                    let typ = [| for p in param -> context.DoubleType |]
                    let ty = LLVMTypeRef.CreateFunction(context.DoubleType, typ) 
                    themodule.AddFunction(name, ty)
                namevalues.[name] <- func
                for i, p in List.indexed param do name_values.[p] <- func.GetParem (uint32 i)
                let bb = func.AppendBasicBloc "entry"
                builder.PositionAtEnd bb 
                let _ = builder.BuilderRet (! body)
                func
            | Extern(name, parem) ->
                let func =
                    let type = [| for p in parem -> context.DoubleType |]
                    let ty = LLVMTypeRef.CreateFunction(context.DoubleType, typ) 
                    themodule.AddFunction(name, ty)
                name_values.[name] <- func
                func
let dump_ir expr =
    let value = codegen expr
    value.Dump()
    printfn ""
