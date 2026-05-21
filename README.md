# F# Kaleidoscope Compiler

An implementation of the classic Kaleidoscope toy language compiler in F#, upgraded to target **.NET 8.0** and utilizing **LLVMSharp 20.1.2** and **FsLexYacc** for lexing/parsing.

This repository serves as a reference for constructing LLVM-based compilers in F# using modern .NET runtimes.

---

## Technical Stack & Dependencies

- **Runtime & Language**: [.NET 8.0 SDK](https://dotnet.microsoft.com/download) / F#
- **Lexer & Parser Generator**: [FsLexYacc](https://github.com/fsprojects/FsLexYacc)
- **LLVM Bindings**: [LLVMSharp 20.1.2](https://github.com/microsoft/LLVMSharp) (utilizing the static `LLVM` functional API)
- **Target LLVM**: LLVM 20.x compatible runtime (via `libLLVM` / `libLLVM.runtime`)

---

## Project Structure

- `AST.fs`: Defines the Abstract Syntax Tree, parsing error context, and error-handling routines.
- `Lexer.fsl`: The FsLex lexer specification.
- `Parser.fsy`: The FsYacc parser specification.
- `Codegen.fs`: The LLVM code generator, converting AST expressions into LLVM IR via LLVMSharp.
- `Program.fs`: Entry point hosting the interactive Read-Eval-Print Loop (REPL).
- `FSharp-Kaleidoscope.fsproj`: The F# project configuration file detailing target framework, package references, and lexer/parser build steps.

---

## Building and Running

Ensure you have the .NET 8.0 SDK installed on your system.

### Build the Compiler
Run the following command to restore packages, compile the lexer/parser specifications, and build the binaries:
```powershell
dotnet build
```

### Run the REPL
Start the interactive compiler prompt:
```powershell
dotnet run
```

---

## Interactive REPL Example

Once running, you can declare extern functions, define custom functions, and evaluate expressions to see the generated LLVM IR in real-time.

```llvm
kaleidoscope> extern sin(x);
declare double @sin(double)

kaleidoscope> def test(x) sin(x) * 2.0;
define double @test(double) {
entry:
  %calltmp = call double @sin(double %0)
  %multmp = fmul double %calltmp, 2.000000e+00
  ret double %multmp
}
```

To exit the REPL, use `Ctrl+C` or input empty lines.
