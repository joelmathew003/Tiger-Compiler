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

    fun plus  a b = Op (a, Plus, b)
    fun minus a b = Op (a, Minus, b)
    fun mul   a b = Op (a, Mul, b)
    fun divi  a b = Op (a, Divi, b)
    fun assign v e = Assignment(v,e)

end