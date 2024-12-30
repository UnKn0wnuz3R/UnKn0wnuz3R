namespace FMinus

open AST

exception TypeError

type Type =
  | Int
  | Bool
  | TyVar of string
  | Func of Type * Type

type Constraint = 
  | Constraint of Type * Type * bool // 마지막 bool 값은 EqConstraint(= or <>)

type Subst = Map<string, Type>
type TypeEnv = Map<string, Type>

module Type =
  let mutable inc = 0

  let MakeTypeVar () =
    inc <- inc + 1
    TyVar (sprintf "t%d" inc)

  let rec toString (typ: Type): string =
    match typ with
    | Int -> "int"
    | Bool -> "bool"
    | TyVar s -> s
    | Func (t1, t2) -> sprintf "(%s) -> (%s)" (toString t1) (toString t2)

  let rec App_Subst (subst: Subst) (typ: Type): Type =
    match typ with
    | Bool -> Bool
    | Int -> Int
    | TyVar v -> 
        if Map.containsKey v subst then App_Subst subst (subst.[v]) else typ
    | Func (t1, t2) -> 
        let new_t1 = App_Subst subst t1
        let new_t2 = App_Subst subst t2
        Func (new_t1, new_t2)

  let rec vaild_TyVar (v: string) (t: Type): bool =
    match t with
    | TyVar tx -> tx = v
    | Func (t1, t2) -> vaild_TyVar v t1 || vaild_TyVar v t2
    | _ -> false

  let eqtyvar_check (t: Type): bool =
    match t with
    | Int -> true
    | Bool -> true
    | _ -> false

  let rec unify (constraints: Constraint list) (s: Subst) (eqvar_set: Set<string>): Subst * Set<string> =
    match constraints with
    | [] -> (s, eqvar_set)
    | Constraint(t1, t2, is_eqtyvar) :: rest ->
        let t1' = App_Subst s t1
        let t2' = App_Subst s t2
        let new_s, new_eqvar_set = 
          if is_eqtyvar then
            let eqVars = 
              match (t1', t2') with
              | TyVar v1, TyVar v2 -> eqvar_set.Add(v1).Add(v2)
              | TyVar v, _ -> eqvar_set.Add(v)
              | _, TyVar v -> eqvar_set.Add(v)
              | _ -> eqvar_set
            let s' = Extend (t1', t2', is_eqtyvar) s
            (s', eqVars)
          else
            let s' = Extend (t1', t2', is_eqtyvar) s
            (s', eqvar_set)
        unify rest new_s new_eqvar_set

  and Extend (t1: Type, t2: Type, is_eqtyvar: bool) (s: Subst): Subst =
    match (t1, t2) with
    | (Int, Int) -> s
    | (Bool, Bool) -> s
    | (Func (tx1, ty1), Func (tx2, ty2)) ->
        let s' = Extend (tx1, tx2, false) s
        Extend (App_Subst s' ty1, App_Subst s' ty2, false) s'
    | (TyVar v, t) | (t, TyVar v) ->
        if t = TyVar v then s
        else if vaild_TyVar v t then raise TypeError
        else Map.add v t s
    | _ -> raise TypeError

  let rec gen (tenv: TypeEnv) (exp: Exp) (t: Type): Constraint list =
    match exp with
    | Num _ -> [Constraint(t, Int, false)]
    | True -> [Constraint(t, Bool, false)]
    | False -> [Constraint(t, Bool, false)]
    | Var x ->
        match Map.tryFind x tenv with
        | Some t' -> [Constraint(t, t', false)]
        | None -> raise TypeError
    | Neg e ->
        let new_const = gen tenv e Int
        Constraint(t, Int, false) :: new_const
    | Add (e1, e2) | Sub (e1, e2) ->
        let new_const1 = gen tenv e1 Int
        let new_const2 = gen tenv e2 Int
        Constraint(t, Int, false) :: new_const1 @ new_const2
    | LessThan (e1, e2) | GreaterThan (e1, e2) ->
        let new_const1 = gen tenv e1 Int
        let new_const2 = gen tenv e2 Int
        Constraint(t, Bool, false) :: new_const1 @ new_const2
    | Equal (e1, e2) | NotEq (e1, e2) ->
        let t1 = MakeTypeVar()
        let new_const1 = gen tenv e1 t1
        let new_const2 = gen tenv e2 t1
        Constraint(t1, t1, true) :: Constraint(t, Bool, false) :: new_const1 @ new_const2
    | IfThenElse (e1, e2, e3) ->
        let new_const1 = gen tenv e1 Bool
        let new_const2 = gen tenv e2 t
        let new_const3 = gen tenv e3 t
        new_const1 @ new_const2 @ new_const3
    | LetIn (x, e1, e2) ->
        let t1 = MakeTypeVar()
        let new_const1 = gen tenv e1 t1
        let newEnv = Map.add x t1 tenv
        let new_const2 = gen newEnv e2 t
        new_const1 @ new_const2
    | LetFunIn (f, x, e1, e2) ->
        let ta = MakeTypeVar()
        let tr = MakeTypeVar()
        let newEnv1 = Map.add x ta tenv
        let new_const1 = gen newEnv1 e1 tr
        let newEnv2 = Map.add f (Func(ta,tr)) tenv
        let new_const2 = gen newEnv2 e2 t
        new_const1 @ new_const2
    | LetRecIn (f, x, e1, e2) ->
        let ta = MakeTypeVar()
        let tr = MakeTypeVar()
        let newEnv1 = Map.add x ta tenv
        let newEnv2 = Map.add f (Func(ta, tr)) newEnv1
        let new_const1 = gen newEnv2 e1 tr
        let newEnv3 = Map.add f (Func(ta, tr)) tenv
        let new_const2 = gen newEnv3 e2 t
        new_const1 @ new_const2
    | Fun (x, e) ->
        let ta = MakeTypeVar()
        let tr = MakeTypeVar()
        let newEnv = Map.add x ta tenv
        let new_const = gen newEnv e tr
        Constraint(t, Func(ta, tr), false) :: new_const
    | App (e1, e2) ->
        let ta = MakeTypeVar()
        let new_const1 = gen tenv e1 (Func (ta, t))
        let new_const2 = gen tenv e2 ta
        new_const1 @ new_const2

  let infer (prog: Program) : Type =
    let t = MakeTypeVar()
    let equations = gen Map.empty prog t
    let total_subst, eqvar_set = unify equations Map.empty Set.empty
    for v in eqvar_set do
        let Program_Type = App_Subst total_subst (TyVar v)
        if not (eqtyvar_check Program_Type) then raise TypeError
    App_Subst total_subst t
