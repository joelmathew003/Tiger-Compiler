structure IR : sig
  type inst = (string, Temp.temp) MIPS.inst
  type stmt = (string, Temp.temp) MIPS.stmt
  type prog = stmt list
  val ppInst : inst -> string
  val ppStmt : stmt -> string
  val pp     : prog -> string
end = struct
  (* complete this *)
end


val assign : Env.t -> Temp.temp -> expr -> IR.prog
val print  : Env.t -> Temp.temp -> IR.prog


signature Temp =
  sig
    type temp
    val newtemp    : unit -> temp
    val tempToString : temp -> string
  end

structure Temp :> TEMP = struct

   type temp  = int (* 2ʷ many variables on a w-sized machine *)
		      (* you can use IntInf.int if you want unbounded *)

   val nextTemp       = ref 0 (* Keep track of how many temps have been allocated *)
   fun newtemp  _     = let nextTemp := !nextTemp +1 in !nextTemp end
   fun tempToString t = "t"^Int.toString t
end

