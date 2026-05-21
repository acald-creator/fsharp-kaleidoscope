#nowarn "9"
#nowarn "3391"
module Codegen

open LLVMSharp.Interop
open AST
open System.Collections.Generic
open Microsoft.FSharp.NativeInterop

let context = LLVM.GetGlobalContext()

let withString (s: string) (f: nativeptr<sbyte> -> 'a) : 'a =
    let bytes = System.Text.Encoding.UTF8.GetBytes(s + "\u0000")
    use p = fixed bytes
    let sptr = p |> NativePtr.toVoidPtr |> NativePtr.ofVoidPtr
    f sptr

let themodule = withString "module" (fun name -> LLVM.ModuleCreateWithNameInContext(name, context))
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
        | "+" -> withString "addtmp" (fun name -> LLVM.BuildFAdd(builder, !lhs, !rhs, name))
        | "-" -> withString "subtmp" (fun name -> LLVM.BuildFSub(builder, !lhs, !rhs, name))
        | "*" -> withString "multmp" (fun name -> LLVM.BuildFMul(builder, !lhs, !rhs, name))
        | "<" ->
            let boolVal = withString "cmptmp" (fun name -> LLVM.BuildFCmp(builder, LLVMRealPredicate.LLVMRealULT, !lhs, !rhs, name))
            withString "booltmp" (fun name -> LLVM.BuildUIToFP(builder, boolVal, LLVM.DoubleTypeInContext(context), name))
        | _ -> failwithf "Invalid Operator %s" op
    | Expr.Call(callee, args) ->
        let calleeVal = !callee
        let argsArray = !!args
        use argsPtr = fixed argsArray
        let fnTy = LLVM.GlobalGetValueType(calleeVal)
        withString "calltmp" (fun name -> LLVM.BuildCall2(builder, fnTy, calleeVal, argsPtr, uint32 argsArray.Length, name))
    | Expr.Func(name, param, body) ->
        let func =
            let typ = [| for _ in param -> LLVM.DoubleTypeInContext(context) |]
            use typPtr = fixed typ
            let ty = LLVM.FunctionType(LLVM.DoubleTypeInContext(context), typPtr, uint32 typ.Length, 0)
            withString name (fun nptr -> LLVM.AddFunction(themodule, nptr, ty))
        namevalues.[name] <- func
        for i, p in List.indexed param do 
            namevalues.[p] <- LLVM.GetParam(func, uint32 i)
        let bb = withString "entry" (fun eptr -> LLVM.AppendBasicBlock(func, eptr))
        LLVM.PositionBuilderAtEnd(builder, bb) 
        let _ = LLVM.BuildRet(builder, !body)
        func
    | Expr.Extern(name, param) ->
        let func =
            let typ = [| for _ in param -> LLVM.DoubleTypeInContext(context) |]
            use typPtr = fixed typ
            let ty = LLVM.FunctionType(LLVM.DoubleTypeInContext(context), typPtr, uint32 typ.Length, 0)
            withString name (fun nptr -> LLVM.AddFunction(themodule, nptr, ty))
        namevalues.[name] <- func
        func

let dump_ir expr =
    let value = codegen expr
    LLVM.DumpValue(value)
    printfn ""
