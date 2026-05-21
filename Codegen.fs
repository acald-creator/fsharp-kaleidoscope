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

LLVM.LinkInMCJIT()
LLVM.InitializeNativeTarget() |> ignore
LLVM.InitializeNativeAsmPrinter() |> ignore
LLVM.InitializeNativeAsmParser() |> ignore

let triple = LLVM.GetDefaultTargetTriple()
let targetMachine =
    let mutable target = Unchecked.defaultof<nativeptr<LLVMTarget>>
    let mutable errorMsg = NativePtr.ofVoidPtr (System.IntPtr.Zero.ToPointer())
    let status = 
        use targetPtr = fixed &target
        use errorPtr = fixed &errorMsg
        LLVM.GetTargetFromTriple(triple, targetPtr, errorPtr)
    if status = 0 then
        let empty = withString "" (fun s -> s)
        let tm = LLVM.CreateTargetMachine(target, triple, empty, empty, LLVMCodeGenOptLevel.LLVMCodeGenLevelDefault, LLVMRelocMode.LLVMRelocDefault, LLVMCodeModel.LLVMCodeModelDefault)
        LLVM.SetTarget(themodule, triple)
        let targetData = LLVM.CreateTargetDataLayout(tm)
        LLVM.SetModuleDataLayout(themodule, targetData)
        tm
    else
        Unchecked.defaultof<LLVMTargetMachineRef>

let passBuilderOptions = LLVM.CreatePassBuilderOptions()

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
        let passesStr = "instcombine,reassociate,gvn,simplifycfg"
        let _ = withString passesStr (fun pptr ->
            LLVM.RunPassesOnFunction(func, pptr, targetMachine, passBuilderOptions)
        )
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

type VoidToDouble = delegate of unit -> double

let jitAndRun (funcVal: LLVMValueRef) =
    let mutable options = LLVMMCJITCompilerOptions()
    let size = System.UIntPtr(uint32 sizeof<LLVMMCJITCompilerOptions>)
    use optionsPtr = fixed &options
    LLVM.InitializeMCJITCompilerOptions(optionsPtr, size)

    let mutable enginePtr : nativeptr<LLVMOpaqueExecutionEngine> = NativePtr.ofVoidPtr (System.IntPtr.Zero.ToPointer())
    let mutable errorMsg : nativeptr<sbyte> = NativePtr.ofVoidPtr (System.IntPtr.Zero.ToPointer())
    
    use enginePtrPtr = fixed &enginePtr
    use errorPtr = fixed &errorMsg
    
    let status = LLVM.CreateMCJITCompilerForModule(enginePtrPtr, themodule, optionsPtr, size, errorPtr)
    let engineRef = new LLVMExecutionEngineRef(System.IntPtr(NativePtr.toVoidPtr enginePtr))
    
    if status <> 0 then
        let msg = System.Runtime.InteropServices.Marshal.PtrToStringAnsi(System.IntPtr(NativePtr.toVoidPtr errorMsg))
        failwithf "JIT Creation Error: %s" msg
        
    let addr = withString "__anon_expr" (fun name -> LLVM.GetFunctionAddress(engineRef, name))
    let funcDelegate = System.Runtime.InteropServices.Marshal.GetDelegateForFunctionPointer<VoidToDouble>(System.IntPtr(int64 addr))
    let res = funcDelegate.Invoke()
    
    let mutable removedModule : nativeptr<LLVMOpaqueModule> = NativePtr.ofVoidPtr (System.IntPtr.Zero.ToPointer())
    use removedModPtr = fixed &removedModule
    let removeStatus = LLVM.RemoveModule(engineRef, themodule, removedModPtr, errorPtr)
    if removeStatus <> 0 then
        printfn "Warning: RemoveModule failed."
        
    LLVM.DisposeExecutionEngine(engineRef)
    LLVM.DeleteFunction(funcVal)
    res
