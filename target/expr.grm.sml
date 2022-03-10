functor ExprLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Expr_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "expr.grm"*)(* This is the preamble where you can have arbitrary sml code. For us
it is empty *)


(*#line 15.1 "expr.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\013\000\002\000\012\000\009\000\011\000\000\000\
\\001\000\001\000\023\000\000\000\
\\001\000\001\000\030\000\000\000\
\\001\000\002\000\009\000\000\000\
\\001\000\003\000\020\000\004\000\019\000\005\000\018\000\008\000\017\000\
\\010\000\028\000\000\000\
\\001\000\006\000\000\000\000\000\
\\001\000\007\000\008\000\000\000\
\\001\000\007\000\031\000\000\000\
\\001\000\007\000\033\000\000\000\
\\001\000\011\000\014\000\000\000\
\\001\000\011\000\016\000\000\000\
\\001\000\014\000\029\000\000\000\
\\001\000\015\000\032\000\000\000\
\\001\000\016\000\035\000\000\000\
\\037\000\000\000\
\\038\000\002\000\007\000\012\000\006\000\013\000\005\000\000\000\
\\039\000\000\000\
\\040\000\000\000\
\\041\000\003\000\020\000\004\000\019\000\005\000\018\000\008\000\017\000\000\000\
\\042\000\003\000\020\000\004\000\019\000\005\000\018\000\008\000\017\000\000\000\
\\043\000\000\000\
\\044\000\000\000\
\\045\000\005\000\018\000\008\000\017\000\000\000\
\\046\000\005\000\018\000\008\000\017\000\000\000\
\\047\000\000\000\
\\048\000\000\000\
\\049\000\000\000\
\"
val actionRowNumbers =
"\015\000\014\000\006\000\003\000\
\\000\000\009\000\015\000\010\000\
\\019\000\000\000\021\000\020\000\
\\000\000\016\000\001\000\000\000\
\\000\000\000\000\000\000\004\000\
\\018\000\011\000\025\000\024\000\
\\023\000\022\000\026\000\002\000\
\\007\000\012\000\008\000\015\000\
\\013\000\017\000\005\000"
val gotoT =
"\
\\002\000\002\000\003\000\001\000\004\000\034\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\008\000\000\000\
\\000\000\
\\002\000\002\000\003\000\013\000\000\000\
\\000\000\
\\000\000\
\\001\000\019\000\000\000\
\\000\000\
\\000\000\
\\001\000\020\000\000\000\
\\000\000\
\\000\000\
\\001\000\022\000\000\000\
\\001\000\023\000\000\000\
\\001\000\024\000\000\000\
\\001\000\025\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\002\000\003\000\032\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 35
val numrules = 13
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | VAR of  (string) | CONST of  (int) | PROGRAM of  (Ast.Stmt list) | STMTS of  (Ast.Stmt list) | STMT of  (Ast.Stmt) | EXP of  (Ast.Expr)
end
type svalue = MlyValue.svalue
type result = Ast.Stmt list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 5) => true | _ => false
val showTerminal =
fn (T 0) => "CONST"
  | (T 1) => "VAR"
  | (T 2) => "PLUS"
  | (T 3) => "MINUS"
  | (T 4) => "MUL"
  | (T 5) => "EOF"
  | (T 6) => "NEWLINE"
  | (T 7) => "DIVI"
  | (T 8) => "OPBRACKET"
  | (T 9) => "CLBRACKET"
  | (T 10) => "ASSIGN"
  | (T 11) => "PRINT"
  | (T 12) => "FOR"
  | (T 13) => "TO"
  | (T 14) => "DO"
  | (T 15) => "DONE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.STMTS STMTS, STMTS1left, STMTS1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 52.38 "expr.grm"*) STMTS (*#line 210.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, STMTS1left, STMTS1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.STMTS ((*#line 54.38 "expr.grm"*)[](*#line 214.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.STMTS STMTS, _, STMTS1right)) :: _ :: ( _, ( MlyValue.STMT STMT, STMT1left, _)) :: rest671)) => let val  result = MlyValue.STMTS ((*#line 55.38 "expr.grm"*)STMT :: STMTS(*#line 218.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, STMT1left, STMTS1right), rest671)
end
|  ( 3, ( ( _, ( _, _, DONE1right)) :: ( _, ( MlyValue.STMTS STMTS, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.CONST CONST2, _, _)) :: _ :: ( _, ( MlyValue.CONST CONST1, _, _)) :: _ :: ( _, ( MlyValue.VAR VAR, _, _)) :: ( _, ( _, FOR1left, _)) :: rest671)) => let val  result = MlyValue.STMT ((*#line 57.71 "expr.grm"*) Ast.for (Ast.Var VAR) (Ast.Const CONST1) (Ast.Const CONST2) STMTS (*#line 222.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, FOR1left, DONE1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.VAR VAR, VAR1left, _)) :: rest671)) => let val  result = MlyValue.STMT ((*#line 58.33 "expr.grm"*) Ast.assign VAR EXP(*#line 226.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, VAR1left, EXP1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: ( _, ( _, PRINT1left, _)) :: rest671)) => let val  result = MlyValue.STMT ((*#line 59.33 "expr.grm"*) Ast.Print EXP(*#line 230.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, PRINT1left, EXP1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.CONST CONST, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 62.33 "expr.grm"*) Ast.Const CONST     (*#line 234.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.VAR VAR, VAR1left, VAR1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 63.33 "expr.grm"*) Ast.Var VAR(*#line 238.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, VAR1left, VAR1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 64.33 "expr.grm"*) Ast.plus  EXP1 EXP2 (*#line 242.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 65.33 "expr.grm"*) Ast.minus EXP1 EXP2 (*#line 246.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 66.33 "expr.grm"*) Ast.mul   EXP1 EXP2 (*#line 250.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 67.33 "expr.grm"*) Ast.divi  EXP1 EXP2 (*#line 254.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( _, _, CLBRACKET1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, OPBRACKET1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 68.36 "expr.grm"*)EXP(*#line 258.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, OPBRACKET1left, CLBRACKET1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.PROGRAM x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Expr_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.CONST i,p1,p2))
fun VAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VAR i,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun NEWLINE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun DIVI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun OPBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun CLBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun DONE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
end
end
