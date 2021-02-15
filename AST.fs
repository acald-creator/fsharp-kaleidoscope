module AST

open FSharp.Text.Parsing

type ParseError(token, shift_tokens, reduce_tokens) =
    inherit exn()
    member __.Data = (token, shift_tokens, reduce_tokens)

let f_parse_error_rich<'t> (ctx: ParseErrorConext<'t>) =
    ignore (raise (ParseError(sprintf "%O" ctx.CurrentToken, ctx.ShiftTokens, ctx.ReduceTokens)))

let f_parse_error_rich = Some f_parse_error_rich

type Expr =
    | Func of string * string list * Expr
    | Extern of string * string list
    | Call of Expr * Expr list
    | Number of float
    | Variable of string
    | Binope of (string * int) * Expr * Expr

let balance_binop (op, lhs, rhs) =
    let ops, prec = op

    match lhs, rhs with
    | Binop ((lops, lprec) as lop, ll, lr), rhs when lprec < prec -> Binop(lop, ll, Binop(op, lr, rhs))
    | lhs, Binop ((rops, rprec) as rop, rl, rr) when rprec < prec -> Binop(rop, Binop(op, lhs, rl), rr)
    | _ -> Binop(op, lhs, rhs)
