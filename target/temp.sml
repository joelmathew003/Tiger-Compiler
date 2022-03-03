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
   fun newtemp  _     = let val t = !nextTemp in nextTemp := t+1; t+1 end
   fun tempToString t = "t"^Int.toString t
end