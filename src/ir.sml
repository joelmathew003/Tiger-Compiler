structure IR : sig
  type inst = (string, Temp.temp) MIPS.inst
  type stmt = (string, Temp.temp) MIPS.stmt
  type prog = stmt list
  val ppInst : inst -> string
  val ppStmt : stmt -> string
  val pp     : prog -> string
end = struct
  fun ppInst (MIPS.Add(a, b, c)) = Temp.tempToString a ^ " := " ^ Temp.tempToString b ^ " + " ^ Temp.tempToString c
    | ppInst (MIPS.Sub(a, b, c)) = Temp.tempToString a ^ " := " ^ Temp.tempToString b ^ " - " ^ Temp.tempToString c
    | ppInst (MIPS.Mul(a, b, c)) = Temp.tempToString a ^ " := " ^ Temp.tempToString b ^ " * " ^ Temp.tempToString c
    | ppInst (MIPS.Div2(a, b, c)) = Temp.tempToString a ^ " := " ^ Temp.tempToString b ^ " / " ^ Temp.tempToString c

  fun ppStmt (MIPS.Inst e)  = ppInst e 

  fun pp []    = ""
  |   pp x::xs = ppStmt(x) ^ "\n" ^ pp(xs)
end