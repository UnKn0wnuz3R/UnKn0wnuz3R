module FMinus

open AST
open Types

// Evaluate expression into a value, under the given environment.
let rec evalExp (exp: Exp) (env: Env) : Val =
  match exp with
  | Num i -> Int i
  | True -> Bool true
  | False -> Bool false
  | Var var -> 
      match Map.tryFind var env with
      | Some value -> value
      | _ -> raise UndefinedSemantics
  | Neg (exp1 : Exp) -> 
      let val1 = evalExp exp1 env
      match val1 with 
      | Int a -> Int (-a)
      | _ -> raise UndefinedSemantics
  | Add (exp1 : Exp, exp2 : Exp) ->
      let val1 = evalExp exp1 env
      let val2 = evalExp exp2 env
      match val1, val2 with
      | Int a, Int b -> Int (a+b)
      | _ -> raise UndefinedSemantics
  | Sub (exp1 : Exp, exp2 : Exp) ->
      let val1 = evalExp exp1 env
      let val2 = evalExp exp2 env
      match val1, val2 with
      | Int a, Int b -> Int (a-b)
      | _ -> raise UndefinedSemantics
  | LessThan (exp1 : Exp, exp2 : Exp) -> 
      let val1 = evalExp exp1 env
      let val2 = evalExp exp2 env
      match val1, val2 with
      | Int a, Int b -> Bool (a < b) 
      | _ -> raise UndefinedSemantics
  | GreaterThan (exp1 : Exp, exp2 : Exp) -> 
      let val1 = evalExp exp1 env
      let val2 = evalExp exp2 env
      match val1, val2 with
      | Int a, Int b -> Bool (a > b) 
      | _ -> raise UndefinedSemantics
  | Equal (exp1 : Exp, exp2 : Exp) ->
      let val1 = evalExp exp1 env
      let val2 = evalExp exp2 env
      match val1, val2 with
      | Int a, Int b -> Bool (a = b) 
      | Bool b1, Bool b2 -> Bool (b1 = b2)
      | _ -> raise UndefinedSemantics
  | NotEq (exp1 : Exp, exp2 : Exp) ->
      let val1 = evalExp exp1 env
      let val2 = evalExp exp2 env
      match val1, val2 with
      | Int a, Int b -> Bool (a <> b) 
      | Bool b1, Bool b2 -> Bool (b1 <> b2)
      | _ -> raise UndefinedSemantics
  | IfThenElse (con : Exp, exp1 : Exp, exp2) ->
      let value = evalExp con env
      match value with 
      | Bool true -> evalExp exp1 env
      | Bool false -> evalExp exp2 env
      | _ -> raise UndefinedSemantics
  | LetIn (var, exp, exp1) ->
      let value = evalExp exp env
      let env1 = Map.add var value env
      evalExp exp1 env1
  | LetFunIn (func, var, exp, exp1) ->
      let func_val = Func(var,exp,env)
      let env1 = Map.add func func_val env
      evalExp exp1 env1  
  | LetRecIn (recfunc, var, exp, exp1) ->
      let recfunc_val = RecFunc(recfunc, var, exp, env)
      let env1 = Map.add recfunc recfunc_val env
      evalExp exp1 env1 
  | Fun (var,exp) -> Func(var,exp,env)
  | App (exp1, exp2) -> 
      let value = evalExp exp1 env 
      let v_arg = evalExp exp2 env
      match value with
      | Func(var,body,env') -> evalExp body (Map.add var v_arg env')
      | RecFunc(recfunc,var,body,env') -> 
        let rec_env = Map.add recfunc (RecFunc(recfunc,var,body,env')) env' // add func mapping 
        evalExp body (Map.add var v_arg rec_env) // and eval
      | _ -> raise UndefinedSemantics 

// Note: You may define more functions.

// The program starts execution with an empty environment. Do not fix this code.
let run (prog: Program) : Val =
  evalExp prog Map.empty
