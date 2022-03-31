signature INST = sig
    type t   (* The type of the instruction *)
    val isJumpLike   : t -> bool
    val isTarget     : t -> bool
end

structure MIPSInst : INST = struct
    type t = (MIPS.Label, MIPS.reg) MIPS.stmt

    fun jumpLike (MIPS.J(_))      = true
    | jumpLike (MIPS.B(_))        = true
    | jumpLike (MIPS.Bczt(_))     = true
    | jumpLike (MIPS.Bczf(_))     = true
    | jumpLike (MIPS.Beq(_,_,_))  = true
    | jumpLike (MIPS.Beqz(_,_))   = true           
    | jumpLike (MIPS.Bge(_,_,_))  = true           
    | jumpLike (MIPS.Bgeu(_,_,_)) = true
    | jumpLike (MIPS.Bgez(_,_))   = true
    | jumpLike (MIPS.Bgezal(_,_)) = true
    | jumpLike (MIPS.Bgt(_,_,_))  = true
    | jumpLike (MIPS.Bgtu(_,_,_)) = true
    | jumpLike (MIPS.Bgtz(_,_))   = true
    | jumpLike (MIPS.Ble(_,_,_))  = true
	| jumpLike (MIPS.Bleu(_,_,_)) = true
	| jumpLike (MIPS.Blez(_,_))   = true
	| jumpLike (MIPS.Bltzal(_,_))   = true
    | jumpLike (MIPS.Blt(_,_,_))  = true
	| jumpLike (MIPS.Bltu(_,_,_)) = true
	| jumpLike (MIPS.Bltz(_,_))   = true
	| jumpLike (MIPS.Bne(_,_,_))  = true
	| jumpLike (MIPS.Bnez(_,_))   = true
	| jumpLike (MIPS.Jal(_))      = true
	| jumpLike (MIPS.Jalr(_))     = true
	| jumpLike (MIPS.Jr(_))       = true
    | jumpLike _                  = false

    fun isJumpLike (MIPS.Inst(x)) = jumpLike(x)
    | isJumpLike _ = false

    fun isTarget (MIPS.label(_)) = true
    | isTarget _ = false
end

functor BasicBlocks (I : INST) = struct

    structure Inst = I                     (* expose the instruction module as well *)
    type block = I.t list

    fun pop [] = []
    |   pop (x::xs) = xs

    fun basicBlocks [] = [[]]
    |   basicBlocks (x::xs) =   if(I.isJumpLike(x) andalso I.isTarget(x)) then []::([x]::basicBlocks xs)
                                else if(I.isJumpLike(x))               
                                then 
                                let
                                  val blist = basicBlocks xs
                                  val curr = hd(blist)
                                  fun conv [] = pop blist
                                  |   conv _  = blist
                                in
                                  [x]::(conv curr)
                                end
                                else if(I.isTarget(x)) 
                                then
                                let
                                    val blist = basicBlocks xs
                                    val curr = hd(blist)
                                    val newcurr = [x]@curr
                                    val newblist = newcurr::pop blist
                                in
                                    []::newblist
                                end
                                else
                                let
                                    val blist = basicBlocks xs
                                    val curr = hd(blist)
                                    val newcurr = [x]@curr
                                in
                                    newcurr::pop blist
                                end

    fun prBlock [] = ""
    |  prBlock (x::xs) = (MIPS.prProg x)^"\n"^(prBlock xs)

end



structure MIPSBasicBlocks = BasicBlocks (MIPSInst)