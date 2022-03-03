signature TEMP =
  sig
    type temp = int
    val newtemp    : unit -> temp
    val tempToString : temp -> string
  end

structure Temp :> TEMP = struct

   type temp  = int (* 2ʷ many variables on a w-sized machine *)
		      (* you can use IntInf.int if you want unbounded *)

   val nextTemp       = ref 2 (* Keep track of how many temps have been allocated *)
   fun newtemp  _     = let 
                            val t = !nextTemp 
                        in 
                            if (t>8) then (TextIO.output(TextIO.stdErr, "Error: Temp registers used up \n"); OS.Process.exit OS.Process.failure)
                            else nextTemp := t+1; t+1 
                        end
   fun tempToString t = "t"^Int.toString t
end