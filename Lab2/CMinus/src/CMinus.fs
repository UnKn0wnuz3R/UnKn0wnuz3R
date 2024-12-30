module CMinus

open AST
open Types

// Evaluate expression into a value, under the given memory.
let rec evalExp (exp: Exp) (mem: Mem) : Val =
  match exp with
  | Num n -> Int n
  | True -> Bool true
  | False -> Bool false
  | Var str -> 
      match Map.tryFind str mem with 
      | Some value -> value
      | None -> raise UndefinedSemantics
  | Add (exp1 : Exp, exp2 : Exp) ->
      let val1 = evalExp exp1 mem
      let val2 = evalExp exp2 mem
      match val1, val2 with
      | Int a, Int b -> Int (a + b)
      | _ -> raise UndefinedSemantics 
  | Sub (exp1 : Exp, exp2 : Exp) -> 
      let val1 = evalExp exp1 mem
      let val2 = evalExp exp2 mem
      match val1, val2 with
      | Int a, Int b -> Int (a - b)
      | _ -> raise UndefinedSemantics  
  | LessThan (exp1 : Exp, exp2 : Exp) -> 
      let val1 = evalExp exp1 mem
      let val2 = evalExp exp2 mem
      match val1, val2 with
      | Int a, Int b -> Bool (a < b) 
      | _ -> raise UndefinedSemantics
  | GreaterThan (exp1 : Exp, exp2 : Exp) -> 
      let val1 = evalExp exp1 mem
      let val2 = evalExp exp2 mem
      match val1, val2 with
      | Int a, Int b -> Bool (a > b) 
      | _ -> raise UndefinedSemantics
  | Equal (exp1 : Exp, exp2 : Exp) -> 
      let val1 = evalExp exp1 mem
      let val2 = evalExp exp2 mem
      match val1, val2 with
      | Int a, Int b -> Bool (a = b) 
      | Bool b1, Bool b2 -> Bool (b1 = b2)
      | _ -> raise UndefinedSemantics
  | NotEq (exp1 : Exp, exp2 : Exp) -> 
      let val1 = evalExp exp1 mem
      let val2 = evalExp exp2 mem
      match val1, val2 with
      | Int a, Int b -> Bool (a <> b) 
      | Bool b1, Bool b2 -> Bool (b1 <> b2)
      | _ -> raise UndefinedSemantics

// Note: You may define more functions.

// Execute a statement and return the updated memory.
let rec exec (stmt: Stmt) (mem: Mem) : Mem =
  match stmt with
  | NOP -> mem // NOP does not change the memory.
  | Assign (var : string, exp : Exp) ->
      let value = evalExp exp mem
      Map.add var value mem
  | Seq (stmt1 : Stmt, stmt2 : Stmt) -> // s1 ; s2
      let temp_mem : Mem = exec stmt1 mem
      exec stmt2 temp_mem
  | If (exp : Exp, stmt1 : Stmt, stmt2 : Stmt) -> // if(exp){stmt1} else{s2}
      let value = evalExp exp mem
      match value with
      | Bool true -> exec stmt1 mem
      | Bool false -> exec stmt2 mem
      | _ -> raise UndefinedSemantics
  | While (exp : Exp, bodyStmt : Stmt) ->
      let value = evalExp exp mem
      match value with
      | Bool true ->
          let temp_mem : Mem = exec bodyStmt mem
          exec (While (exp, bodyStmt)) temp_mem
      | Bool false -> mem
      | _ -> raise UndefinedSemantics

// The program starts execution with an empty memory. Do NOT fix this function.
let run (prog: Program) : Mem =
  exec prog Map.empty
