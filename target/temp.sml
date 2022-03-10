signature TEMP =
  sig
    type temp = int
    type label = int
    val newlabel   : unit -> label
    val newtemp    : unit -> temp
    val labelToString : label -> string
    val tempToString : temp -> string
  end

structure Temp :> TEMP = struct

  type label = int
  type temp  = int (* 2ʷ many variables on a w-sized machine *)
          (* you can use IntInf.int if you want unbounded *)

  val nextTemp       = ref 0 (* Keep track of how many temps have been allocated *)
  val nextLabel      = ref 0
  fun newtemp  _     = let 
                            val t = !nextTemp 
                        in 
                            if (t>8) then (TextIO.output(TextIO.stdErr, "Error: Temp registers used up \n"); OS.Process.exit OS.Process.failure)
                            else nextTemp := t+1; t 
                        end
  fun tempToString t = "t"^Int.toString t

  fun newlabel _ = let 
                    val l = !nextLabel 
                  in 
                    nextLabel := l+1; l
                  end
  fun labelToString l = "l"^Int.toString l

end