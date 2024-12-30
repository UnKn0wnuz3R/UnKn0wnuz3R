module CMinusPtr

open AST
open Types

// Evaluate expression into a value, under the given memory.
let rec evalExp (exp: Exp) (mem: Mem) : Val =
  let rec evalLVal l_value =
      match l_value with
      | Var var ->
          match Map.tryFind var mem with
          | Some value -> value
          | None -> raise UndefinedSemantics
      | Deref temp_exp ->
          let result = evalExp temp_exp mem    
          match result with
          | Loc loc ->
              match Map.tryFind loc mem with
              | Some value -> value
              | None -> raise UndefinedSemantics
          | _ -> raise UndefinedSemantics

  match exp with
  | Num i -> Int i
  | True -> Bool true
  | False -> Bool false
  | LV l_value -> evalLVal l_value 
  | AddrOf var -> Loc var
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
      | Loc l1, Loc l2 -> Bool (l1 = l2)
      | _ -> raise UndefinedSemantics
  | NotEq (exp1 : Exp, exp2 : Exp) ->
      let val1 = evalExp exp1 mem
      let val2 = evalExp exp2 mem
      match val1, val2 with
      | Int a, Int b -> Bool (a <> b) 
      | Bool b1, Bool b2 -> Bool (b1 <> b2)
      | Loc l1, Loc l2 -> Bool (l1 <> l2)
      | _ -> raise UndefinedSemantics

  
// Execute a statement and return the updated memory.
let rec exec (stmt: Stmt) (mem: Mem) : Mem =
  match stmt with
  | NOP -> mem // NOP does not change the memory.
  | Assign (Var var, exp : Exp) ->
      let value = evalExp exp mem
      Map.add var value mem
  | Assign (Deref e, exp : Exp) ->
      let value = evalExp exp mem // rv
      let val_loc = evalExp e mem // loc  
      match val_loc with
      | Loc loc -> Map.add loc value mem
      | _ -> raise UndefinedSemantics
  | Seq (stmt1 : Stmt, stmt2 : Stmt) ->
      let temp_mem : Mem = exec stmt1 mem
      exec stmt2 temp_mem
  | If (exp : Exp, stmt1 : Stmt, stmt2 : Stmt) ->
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
