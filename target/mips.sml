structure MIPS = struct

(* The registers of the mips machine *)
datatype reg =  zero | at | v0 | v1 | 
                a0 | a1 | a2 | a3 | 
                t0 | t1 | t2 | t3 | t4 | t5 | t6 | t7 | 
                s0 | s1 | s2 | s3 | s4 | s5 | s6 | s7 | 
                t8 | t9 | 
                k0 | k1 | 
                gp | sp | fp | ra |
                Imm of int

(* The instruction *)
datatype ('l, 't) inst =  Abs  	of 't * 't                          (*The Arithmetic and Logical Operations*)
                        | Add  	of 't * 't * 't
                        | Addi 	of 't * 't * 't
                        | Addu  of 't * 't * 't
                        | Addiu of 't * 't * 't
                        | And  	of 't * 't * 't
                        | Andi  of 't * 't * 't
                        | Div  	of 't * 't
                        | Divu  of 't * 't
                        | Div2  of 't * 't * 't
                        | Divu2 of 't * 't * 't
                        | Mul  	of 't * 't * 't
                        | Mulo  of 't * 't * 't
                        | Mulou of 't * 't * 't
                        | Mult  of 't * 't
                        | Multu of 't * 't
                        | Neg  	of 't * 't
                        | Negu  of 't * 't
                        | Nor  	of 't * 't * 't
                        | Not  	of 't * 't
                        | Or  	of 't * 't * 't
                        | Ori 	of 't * 't * 't
                        | Rem  	of 't * 't * 't
                        | Remu  of 't * 't * 't
                        | Rol  	of 't * 't * 't
                        | Ror  	of 't * 't * 't
                        | Sll  	of 't * 't * 't
                        | Sllv  of 't * 't * 't
                        | Sra 	of 't * 't * 't
                        | Srav  of 't * 't * 't
                        | Srl  	of 't * 't * 't
                        | Srlv  of 't * 't * 't
                        | Sub  	of 't * 't * 't
                        | Subu  of 't * 't * 't
                        | Xor  	of 't * 't * 't
                        | Xori 	of 't * 't * 't                     
                        | Li 	of 't * 't                          (* Constant-Manipulating Instructions *)
                        | Lui 	of 't * 't                          
                        | Seq 	of 't * 't * 't                     (* Comparison Instructions *)
                        | Sge 	of 't * 't * 't
                        | Sgeu 	of 't * 't * 't
                        | Sgt 	of 't * 't * 't
                        | Sgtu 	of 't * 't * 't
                        | Sle 	of 't * 't * 't
                        | Sleu 	of 't * 't * 't
                        | Slt 	of 't * 't * 't
                        | Slti 	of 't * 't * 't
                        | Sltu 	of 't * 't * 't
                        | Sltiu of 't * 't * 't
                        | Sne 	of 't * 't * 't                     
                        | B 	of 'l                               (* Branch and Jump Instructions *)
                        | Bczt 	of 'l
                        | Bczf 	of 'l
                        | Beq 	of 't * 't * 'l
                        | Beqz 	of 't * 'l
                        | Bge  	of 't * 't * 'l
                        | Bgeu 	of 't * 't * 'l 
                        | Bgez 	of 't * 'l
                        | Bgezal of 't * 'l
                        | Bgt 	of 't * 't * 'l
                        | Bgtu 	of 't * 't * 'l
                        | Bgtz 	of 't * 'l
                        | Ble 	of 't * 't * 'l
                        | Bleu 	of 't * 't * 'l
                        | Blez 	of 't * 'l
                        | Bltzal of 't * 'l
                        | Blt 	of 't * 't * 'l
                        | Bltu 	of 't * 't * 'l
                        | Bltz 	of 't * 'l
                        | Bne 	of 't * 't * 'l
                        | Bnez 	of 't * 'l
                        | J 	of 'l
                        | Jal 	of 'l
                        | Jalr 	of 't
                        | Jr 	of 't                               
                        | La 	of 't * 'l                          (* Load Instructions *)
                        | Lb 	of 't * 'l
                        | Lbu 	of 't * 'l
                        | Ld 	of 't * 'l
                        | Lh 	of 't * 'l
                        | Lhu 	of 't * 'l
                        | Lw 	of 't * 'l
                        | Lwcz 	of 't * 'l
                        | Lwl 	of 't * 'l
                        | Lwr 	of 't * 'l                          
                        | Sb 	of 't * 'l                          (* Store Instructions *)
                        | Sd 	of 't * 'l
                        | Sh 	of 't * 'l
                        | Sw 	of 't * 'l
                        | Swcz 	of 't * 'l
                        | Swl 	of 't * 'l
                        | Swr 	of 't * 'l
                        | Ulh 	of 't * 'l
                        | Ulhu 	of 't * 'l
                        | Ulw 	of 't * 'l
                        | Ush 	of 't * 'l
                        | Usw 	of 't * 'l                          
                        | Rfe                                       (* Trap and Exception Instructions *)
                        | Syscall
                        | Break of int
                        | Nop                                       
                        | Move of 't * 't                           (* Data Movement Instructions *)
                        | Mfhi of 't                                
                        | Mflo of 't
                        | Mthi of 't
                        | Mtlo of 't
                        | Mfcz of 't * 't
                        | Mtcz of 't * 't

(* The instructions and assembler directives *)
datatype direc  = align of int
                | ascii of string
                | asciiz of string
                | byte of int list
                | data of string
                | extern of string*int
                | globl of string
                | half of int list
                | kdata of string
                | ktext of string
                | space of int
                | text of string
                | word of int list
            
datatype Label = UserDefined of string  (* main, fib *)
               | TempLabel   of int     (* $L0 $L1 ... *)

(* The instructions and assembler directives *)
datatype ('l,'t) stmt =   Inst of ('l, 't) inst
                        | Direc of direc
                        | label of Label

fun   prReg zero = "$zero"
    | prReg at   = "$at"
    | prReg v0   = "$v0"
    | prReg v1   = "$v1"
    | prReg a0   = "$a0"
    | prReg a1   = "$a1"
    | prReg a2   = "$a2"
    | prReg a3   = "$a3"
    | prReg t0   = "$t0"
    | prReg t1   = "$t1"
    | prReg t2   = "$t2"
    | prReg t3   = "$t3"
    | prReg t4   = "$t4"
    | prReg t5   = "$t5"
    | prReg t6   = "$t6"
    | prReg t7   = "$t7"
    | prReg s0   = "$s0"
    | prReg s1   = "$s1"
    | prReg s2   = "$s2"
    | prReg s3   = "$s3"
    | prReg s4   = "$s4"
    | prReg s5   = "$s5"
    | prReg s6   = "$s6"
    | prReg s7   = "$s7"
    | prReg t8   = "$t8"
    | prReg t9   = "$t9"
    | prReg k0   = "$k0"
    | prReg k1   = "$k1"
    | prReg gp   = "$gp"
    | prReg sp   = "$sp"
    | prReg fp   = "$fp"
    | prReg ra   = "$ra"
    | prReg (Imm (i)) = Int.toString(i)

fun   prLabel (UserDefined s) = s^":"
    | prLabel (TempLabel i)  = "$L"^Int.toString i^":"

(* 
Print the instructions when the labels are strings and
registers are actual MIPS registers
*)
fun     prInst (Abs(r1,r2))       = "abs "^prReg(r1)^", "^prReg(r2)
    |   prInst (Add(r1,r2,r3))    = "add "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Addi(r1,r2,r3))   = "addi "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Addu(r1,r2,r3))   = "addu "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Addiu(r1,r2,r3))  = "addiu "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (And(r1,r2,r3))    = "and "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Andi(r1,r2,r3))   = "andi "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Div(r1,r2))       = "div "^prReg(r1)^", "^prReg(r2)
    |   prInst (Divu(r1,r2))      = "divu "^prReg(r1)^", "^prReg(r2)
    |   prInst (Div2(r1,r2,r3))   = "div "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Divu2(r1,r2,r3))  = "divu "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Mul(r1,r2,r3))    = "mul "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Mulo(r1,r2,r3))   = "mulo "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Mulou(r1,r2,r3))  = "mulou "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Mult(r1,r2))      = "mult "^prReg(r1)^", "^prReg(r2)
    |   prInst (Multu(r1,r2))     = "multu "^prReg(r1)^", "^prReg(r2)
    |   prInst (Neg(r1,r2))       = "neg "^prReg(r1)^", "^prReg(r2)
    |   prInst (Negu(r1,r2))      = "negu "^prReg(r1)^", "^prReg(r2)
    |   prInst (Nor(r1,r2,r3))    = "nor "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Not(r1,r2))       = "not "^prReg(r1)^", "^prReg(r2)
    |   prInst (Or(r1,r2,r3))     = "or "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Ori(r1,r2,r3))    = "ori "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Rem(r1,r2,r3))    = "rem "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Remu(r1,r2,r3))   = "remu "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Rol(r1,r2,r3))    = "rol "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Ror(r1,r2,r3))    = "ror "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Sll(r1,r2,r3))    = "sll "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Sllv(r1,r2,r3))   = "sllv "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Sra(r1,r2,r3))    = "sra "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Srav(r1,r2,r3))   = "srav "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Srl(r1,r2,r3))    = "srl "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Srlv(r1,r2,r3))   = "srlv "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Sub(r1,r2,r3))    = "sub "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Subu(r1,r2,r3))   = "subu "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Xor(r1,r2,r3))    = "xor "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Xori(r1,r2,r3))   = "xori "^prReg(r1)^", "^prReg(r2)^", "^prReg(r3)
    |   prInst (Li(r1,r2))              = "li "^prReg(r1)^", "^prReg(r2)                                        
    |   prInst (Lui(r1,r2))             = "lui "^prReg(r1)^", "^prReg(r2) 
    |   prInst (Seq(r1, r2, r3)) 	    = "seq "  ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prReg(r3)      
    |   prInst (Sge(r1, r2, r3))		= "sge "  ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prReg(r3)
    |   prInst (Sgeu(r1, r2, r3))		= "sgeu " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prReg(r3)
    |   prInst (Sgt(r1, r2, r3))		= "sgt "  ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prReg(r3)
    |   prInst (Sgtu(r1, r2, r3)) 		= "sgtu " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prReg(r3)
    |   prInst (Sle(r1, r2, r3)) 		= "sle "  ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prReg(r3)
    |   prInst (Sleu(r1, r2, r3)) 		= "sleu " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prReg(r3)
    |   prInst (Slt(r1, r2, r3)) 		= "slt "  ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prReg(r3)
    |   prInst (Slti(r1, r2, r3)) 		= "slti " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prReg(r3)
    |   prInst (Sltu(r1, r2, r3)) 		= "sltu " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prReg(r3)
    |   prInst (Sltiu(r1, r2, r3)) 	    = "sltui " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prReg(r3)
    |   prInst (Sne(r1, r2, r3)) 		= "sne " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prReg(r3)
    |   prInst (B(l1)) 				    = "b " ^ prLabel(l1)
    |   prInst (Bczt(l1)) 				= "bczt " ^ prLabel(l1)
    |   prInst (Bczf(l1)) 				= "bczf " ^ prLabel(l1)
    |   prInst (Beq(r1, r2, l1)) 		= "beq " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prLabel(l1)
    |   prInst (Beqz(r1, l1)) 			= "beqz " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Bge(r1, r2, l1)) 		= "bge " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prLabel(l1)
    |   prInst (Bgeu(r1, r2, l1)) 	    = "bgeu " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prLabel(l1)
    |   prInst (Bgez(r1, l1)) 			= "bgez " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Bgezal(r1, l1)) 		= "bgezal " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Bgt(r1, r2, l1)) 		= "bgt " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prLabel(l1)
    |   prInst (Bgtu(r1, r2, l1)) 		= "bgtu " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prLabel(l1)
    |   prInst (Bgtz(r1, l1)) 			= "bgtz " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Ble(r1, r2, l1)) 		= "ble " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prLabel(l1)
    |   prInst (Bleu(r1, r2, l1)) 	    = "bleu " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prLabel(l1)
    |   prInst (Blez(r1, l1)) 			= "blez " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Bltzal(r1, l1)) 		= "bltzal " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Blt(r1, r2, l1)) 		= "blt " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prLabel(l1)
    |   prInst (Bltu(r1,r2, l1))        = "bltu " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prLabel(l1)
    |   prInst (Bltz(r1, l1)) 			= "bltz " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Bne(r1, r2, l1)) 		= "bne " ^ prReg(r1) ^ ", " ^ prReg(r2) ^ ", " ^ prLabel(l1)
    |   prInst (Bnez(r1, l1)) 			= "bnez " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (J(l1)) 				    = "j " ^ prLabel(l1)
    |   prInst (Jal(l1)) 			    = "jal " ^ prLabel(l1)
    |   prInst (Jalr(r1)) 			    = "jalr " ^ prReg(r1)
    |   prInst (Jr(r1)) 			    = "jr " ^ prReg(r1)
    |   prInst (La(r1, l1))		        = "la " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Lb(r1, l1))		        = "lb " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Lbu(r1, l1))		    = "lbu " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Ld(r1, l1))		        = "ld " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Lh(r1, l1))		        = "lh " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Lhu(r1, l1))		    = "lhu " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Lw(r1, l1))		        = "lw " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Lwcz(r1, l1))		    = "lwcz " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Lwl(r1, l1))		    = "lwl " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Lwr(r1, l1))		    = "lwr " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Sb(r1, l1))		        = "sb " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Sd(r1, l1))		        = "sd " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Sh(r1, l1))		        = "sh " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Sw(r1, l1))		        = "sw " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Swcz(r1, l1))		    = "swcz " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Swl(r1, l1))		    = "swl " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Swr(r1, l1))		    = "swr " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Ulh(r1, l1))		    = "ulh " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Ulhu(r1, l1))		    = "ulhu " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Ulw(r1, l1))		    = "ulw " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Ush(r1, l1))		    = "ush " ^ prReg(r1) ^ ", " ^ prLabel(l1)
    |   prInst (Usw(r1, l1))		    = "usw " ^ prReg(r1) ^ ", " ^ prLabel(l1)   
    |   prInst (Rfe)                    = "rfe"
    |   prInst (Syscall)                = "syscall"
    |   prInst (Break(i))               = "break "^Int.toString(i)
    |   prInst (Nop)                    = "nop"  
    |   prInst (Move (r1, r2))          = "move "^ prReg(r1)^", "^prReg(r2) 
    |   prInst (Mfhi (l1))              = "mfhi "^ prReg(l1)
    |   prInst (Mflo (l1))              = "mflo "^ prReg(l1)
    |   prInst (Mthi (l1))              = "mthi "^ prReg(l1)
    |   prInst (Mtlo (l1))              = "mtlo "^ prReg(l1)
    |   prInst (Mfcz (l1, l2))          = "mfcz "^ prReg(l1)^", "^prReg(l2)
    |   prInst (Mtcz (l1, l2))          = "mtcz "^ prReg(l1)^", "^prReg(l2)

fun prList []     = ""
  | prList (x::[]) = Int.toString x
  | prList (x::xs) = (Int.toString x) ^ ", "^(prList xs)

fun   prDirec (align(n))   =   ".align "^Int.toString(n)
    | prDirec (ascii(n))   =   ".ascii "^n
    | prDirec (asciiz(n))  =   ".asciiz "^n
    | prDirec (byte(n))    =   ".byte "^prList(n)
    | prDirec (data(n))    =   ".data "^n
    | prDirec (extern(s,n))=   ".extern "^s^" "^Int.toString(n)
    | prDirec (globl(s))   =   ".globl "^s
    | prDirec (half(x))    =   ".half "^prList(x)
    | prDirec (kdata(s))   =   ".kdata "^s
    | prDirec (ktext(s))   =   ".ktext "^s
    | prDirec (space(n))   =   ".space "^Int.toString(n)
    | prDirec (text(s))    =   ".text "^s
    | prDirec (word(x))    =   ".word "^prList(x)

fun   prStmt (Inst(i))  = prInst i
    | prStmt (Direc(d)) = prDirec d
    | prStmt (label(l)) = prLabel l 
(* actual code that SPIM can understand is (string, reg) inst *)

fun prProg [] = ""
|   prProg (x::xs) = (prStmt x)^"\n"^(prProg xs)
end
