type faexpr = 
    | FANil
    | FABool of bool
    | FAInt of int
    | FAString of string
    | FATransition of char
    | FAVar of string
    | FAAdd of faexpr * faexpr
    | FASub of faexpr * faexpr
    | FAMul of faexpr * faexpr
    | FADiv of faexpr * faexpr
    | FALT of faexpr * faexpr
    | FAGT of faexpr * faexpr
    | FAGE of faexpr * faexpr
    | FALE of faexpr * faexpr
    | FAEQ of faexpr * faexpr
    | FAApply of faexpr * faexpr

type favar = (string * faexpr)
type fastate = favar list

type facmd =
    | FAAssign of string * faexpr
    | FASeq of facmd * facmd
    | FAIfElse of faexpr * facmd * facmd
    | FAIf of faexpr * facmd
    | FAWhile of faexpr * facmd 

type faprog = (facmd * fastate)

let rec vars_of_exp : faexpr -> string list = fun cmd ->
    match cmd with
    | FANil -> []
    | FABool _ -> []
    | FAInt _ -> []
    | FAString _ -> []
    | FATransition _ -> []
    | FAVar x -> [x]
    | FAAdd(e1, e2) -> (vars_of_exp e1) @ (vars_of_exp e2)
    | FASub(e1, e2) -> (vars_of_exp e1) @ (vars_of_exp e2)
    | FAMul(e1, e2) -> (vars_of_exp e1) @ (vars_of_exp e2)
    | FADiv(e1, e2) -> (vars_of_exp e1) @ (vars_of_exp e2)
    | FALT(e1, e2) -> (vars_of_exp e1) @ (vars_of_exp e2)
    | FAGT(e1, e2) -> (vars_of_exp e1) @ (vars_of_exp e2)
    | FAGE(e1, e2) -> (vars_of_exp e1) @ (vars_of_exp e2)
    | FALE(e1, e2) -> (vars_of_exp e1) @ (vars_of_exp e2)
    | FAEQ(e1, e2) -> (vars_of_exp e1) @ (vars_of_exp e2)
    | FAApply(e1, e2) -> (vars_of_exp e1) @ (vars_of_exp e2)

let rec vars_of_cmd : facmd -> string list = fun cmd ->
    match cmd with
    | FAAssign(v, e) -> v::(vars_of_exp e)
    | FASeq(c1, c2) -> (vars_of_cmd c1) @ (vars_of_cmd c2)
    | FAIfElse(exp, c1, c2) -> (vars_of_exp exp) @ (vars_of_cmd c1) @ (vars_of_cmd c2)
    | FAIf(exp, c) -> (vars_of_exp exp) @ (vars_of_cmd c)
    | FAWhile(exp, c) -> (vars_of_exp exp) @ (vars_of_cmd c)

let vars_of : facmd -> string list = fun cmd -> Util.unique_elements (vars_of_cmd cmd)

let rec lookup : string -> fastate -> faexpr = fun name st ->
    match st with
    | [] -> FANil
    | (vname, expr)::t -> if vname = name then expr else lookup name t

let rec update : string -> faexpr -> fastate -> fastate = fun name expr st ->
    match st with
    | [] -> [(name, expr)]
    | (vname, expr)::t -> if vname = name then (name, expr)::(update name expr t) else (vname, expr)::(update name expr t)

let rec interpret_cmd : facmd -> fastate -> fastate = fun cmd st ->
    match cmd with
    | FAAssign(v, e) -> (update v e st)
    | FASeq(c1, c2) -> (interpret_cmd c2 (interpret_cmd c1 st))
    | FAIfElse(ex, c1, c2) -> []

