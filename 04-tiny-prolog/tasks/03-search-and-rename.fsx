// ----------------------------------------------------------------------------
// 03 - Searching for clauses & variable renaming
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Term>) term = 
  match term with 
  | Atom _ -> term
  | Variable v -> 
      match subst.TryFind v with 
      | Some t -> t
      | None -> term
  | Predicate(p, args) -> 
      Predicate(p, List.map (substitute subst) args)


let substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) = 
  List.map (fun (v, t) -> v, substitute newSubst t) subst

let substituteTerms subst (terms:list<Term>) = 
  List.map (substitute subst) terms

let rec unifyLists l1 l2 = 
  match l1, l2 with 
  | [], [] -> 
      Some []
  | h1::t1, h2::t2 -> 
      match unify h1 h2 with
      | Some s1 -> 
          let t1 = substituteTerms (Map.ofList s1) t1
          let t2 = substituteTerms (Map.ofList s1) t2
          match unifyLists t1 t2 with
          | Some s2 -> 
              let s1 = substituteSubst (Map.ofList s2) s1
              Some (s1 @ s2)
          | None -> None
      | None -> None
  | _ -> 
    None

and unify t1 t2 = 
  match t1, t2 with 
  | Atom a1, Atom a2 when a1 = a2 ->
      Some []
  | Predicate(p1, args1), Predicate(p2, args2) when p1 = p2 ->
      unifyLists args1 args2
  | Variable v, t | t, Variable v ->
      Some [(v, t)]
  | _ ->
      None

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term = 
  match term with 
  | Atom _ -> []
  | Variable v -> [v]
  | Predicate(_, args) -> List.collect freeVariables args


let withFreshVariables (clause:Clause) : Clause =
  let vars = List.distinct (freeVariables clause.Head @ List.collect freeVariables clause.Body)
  let subst = Map.ofList [ for v in vars -> v, Variable(v + string (nextNumber())) ]
  { Head = substitute subst clause.Head
    Body = substituteTerms subst clause.Body }


let query (program:list<Clause>) (query:Term) 
    : list<Clause * list<string * Term>> =
  [ for clause in program do
      let clause = withFreshVariables clause
      match unify clause.Head query with
      | Some subst -> yield clause, subst
      | None -> () ]


// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

// Generating fresh variables - repeated calls
// should append new number to all variable names
rule (Predicate("grandparent", [Variable("X"); Variable("Y")])) [
  Predicate("parent", [Variable("X"); Variable("Z")])
  Predicate("parent", [Variable("Z"); Variable("Y")]) ]
|> withFreshVariables

// Some information about the British royal family 
let family = [ 
  fact (Predicate("male", [Atom("William")]))
  fact (Predicate("female", [Atom("Diana")]))
  fact (Predicate("male", [Atom("Charles")]))
  fact (Predicate("male", [Atom("George")]))
  fact (Predicate("parent", [Atom("Diana"); Atom("William")]))
  fact (Predicate("parent", [Atom("Charles"); Atom("William")]))
  fact (Predicate("parent", [Atom("William"); Atom("George")]))
  rule (Predicate("father", [Variable("X"); Variable("Y")])) [
    Predicate("parent", [Variable("X"); Variable("Y")])
    Predicate("male", [Variable("X")])
  ]
]

// Query: male(X)
// Match #1: male(William)
// Match #2: male(Charles)
// Match #3: male(George)
query family (Predicate("male", [Variable("X")]))

// Query: father(X, William)
// Match #1: father(X, Y) :- parent(X, Y), male(X)
query family (Predicate("father", [Variable("X"); Atom("William")]))
