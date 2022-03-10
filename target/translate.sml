(* open Atom
open AtomMap *)
structure Translate =
struct

fun tempToReg 0 = MIPS.t0
  | tempToReg 1 = MIPS.t1
  | tempToReg 2 = MIPS.t2
  | tempToReg 3 = MIPS.t3
  | tempToReg 4 = MIPS.t4
  | tempToReg 5 = MIPS.t5
  | tempToReg 6 = MIPS.t6
  | tempToReg 7 = MIPS.t7
  | tempToReg _ = MIPS.Imm(1)

fun tempToLabel n = MIPS.TempLabel(n)

fun compileExpr env t (Ast.Const x) = ([MIPS.Inst(MIPS.Li(tempToReg(t),MIPS.Imm(x)))], env)
  | compileExpr env t (Ast.Var v) = let
                                      val value = case AtomMap.find(env, Atom.atom(v)) of
                                      (SOME(u)) => ([MIPS.Inst(MIPS.Move(tempToReg(t), tempToReg(u)))], env)
                                      |NONE =>  let
                                                  val tnew = Temp.newtemp()
                                                  val env1 = AtomMap.insert(env,Atom.atom(v),tnew)
                                                in  
                                                  ([], env1)
                                                end
                                    in
                                      value
                                    end
  | compileExpr env t (Ast.Op(e1,Ast.Plus,e2)) =  let 
                                                    val t1 = Temp.newtemp()
                                                    val t2 = Temp.newtemp()
                                                    val (res1, env1) = compileExpr env t1 e1
                                                    val (res2, env2) = compileExpr env1 t2 e2
                                                  in
                                                    (res1@res2@[MIPS.Inst(MIPS.Add(tempToReg(t), tempToReg(t1), tempToReg(t2)))], env2)
                                                  end
  | compileExpr env t (Ast.Op(e1,Ast.Minus,e2)) =  let 
                                                    val t1 = Temp.newtemp()
                                                    val t2 = Temp.newtemp()
                                                    val (res1, env1) = compileExpr env t1 e1
                                                    val (res2, env2) = compileExpr env1 t2 e2
                                                  in
                                                    (res1@res2@[MIPS.Inst(MIPS.Sub(tempToReg(t), tempToReg(t1), tempToReg(t2)))], env2)
                                                  end
  | compileExpr env t (Ast.Op(e1,Ast.Mul,e2)) =  let 
                                                    val t1 = Temp.newtemp()
                                                    val t2 = Temp.newtemp()
                                                    val (res1, env1) = compileExpr env t1 e1
                                                    val (res2, env2) = compileExpr env1 t2 e2
                                                  in
                                                    (res1@res2@[MIPS.Inst(MIPS.Mul(tempToReg(t), tempToReg(t1), tempToReg(t2)))], env2)
                                                  end
  | compileExpr env t (Ast.Op(e1,Ast.Divi,e2)) =  let 
                                                    val t1 = Temp.newtemp()
                                                    val t2 = Temp.newtemp()
                                                    val (res1, env1) = compileExpr env t1 e1
                                                    val (res2, env2) = compileExpr env1 t2 e2
                                                  in
                                                    (res1@res2@[MIPS.Inst(MIPS.Div2(tempToReg(t), tempToReg(t1), tempToReg(t2)))], env2)
                                                  end


fun compileStmt env (Ast.Assignment(id,e)) =  let
                                            val value = case AtomMap.find(env, Atom.atom(id)) of
                                              (SOME(u)) => let
                                                              val (res1, env1) = compileExpr env u e
                                                            in
                                                              (res1, env)
                                                            end
                                              |NONE =>  let
                                                          val tnew = Temp.newtemp()
                                                          val env1 = AtomMap.insert(env,Atom.atom(id),tnew)
                                                          val (res1, env2) = compileExpr env1 tnew e
                                                        in  
                                                          (res1, env2)
                                                        end
                                            in
                                              value
                                            end
  | compileStmt env (Ast.Print (e)) = let 
                                        val t1 = Temp.newtemp()
                                        val (res,env1) =  compileExpr env t1 e
                                        (* val env2 = AtomMap.insert(env1,Atom.atom("a0"),t1) *)
                                      in
                                        (res@[MIPS.Inst(MIPS.Move(MIPS.a0,tempToReg(t1)))]@[MIPS.Inst(MIPS.Li(MIPS.v0,MIPS.Imm(1)))]@[MIPS.Inst(MIPS.Syscall)], env1)
                                      end

  | compileStmt env (Ast.For(v,c1,c2,stmtList)) =  let
                                                      val loopTemp = Temp.newtemp()
                                                      val loopEnv = AtomMap.insert(env,Atom.atom(v),loopTemp)
                                                      val stopTemp = Temp.newtemp()
                                                      val loopLabel = Temp.newlabel()
                                                      val exitLabel = Temp.newlabel()
                                                      fun loopCompile e [] = []
                                                        | loopCompile e (x::xs) = let
                                                                                      val (res,env1) = compileStmt e x
                                                                                    in
                                                                                      res@loopCompile env1 xs
                                                                                    end
                                                      val loopStmts = loopCompile loopEnv stmtList
                                                    in
                                                      ([MIPS.Inst(MIPS.Li(tempToReg(loopTemp),MIPS.Imm(c1))), MIPS.Inst(MIPS.Li(tempToReg(stopTemp),MIPS.Imm(c2))),
                                                        MIPS.label(tempToLabel(loopLabel)),
                                                        MIPS.Inst(MIPS.Bgt(tempToReg(loopTemp), tempToReg(stopTemp), tempToLabel(exitLabel)))]
                                                      @loopStmts
                                                      @[MIPS.Inst(MIPS.Addi(tempToReg(loopTemp),tempToReg(loopTemp),MIPS.Imm(1))),
                                                        MIPS.Inst(MIPS.J(tempToLabel(loopLabel))),
                                                        MIPS.label(tempToLabel(exitLabel))], env)
                                                    end


fun compileProg env [] = [MIPS.Inst(MIPS.Li(MIPS.v0,MIPS.Imm(10)))]@[MIPS.Inst(MIPS.Syscall)]
  | compileProg env (x::xs) = let
                              val (res,env1) = compileStmt env x
                            in
                              res@compileProg env1 xs
                            end
                
fun compile prog = [MIPS.Direc(MIPS.globl("main")), MIPS.label(MIPS.UserDefined("main"))]@compileProg AtomMap.empty prog

end




