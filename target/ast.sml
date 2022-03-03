structure Ast = struct
    datatype Expr  = Const of int
                    | Var of string
                    | Op  of Expr * BinOp * Expr

     and BinOp = Plus
               | Minus
               | Mul
               | Divi

    datatype Stmt   = Assignment of string*Expr
                    | Print of Expr
      
    datatype prog = EXPS of (Stmt list)

end