// ----------------------------------------------------------------------------
// 04 - Generating and solving goals recursively
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
  failwith "implemented in step 2"

let substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) = 
  failwith "implemented in step 2"

let substituteTerms subst (terms:list<Term>) = 
  failwith "implemented in step 2"

let rec unifyLists l1 l2 = 
  failwith "implemented in steps 1 and 2"

and unify t1 t2 = 
  failwith "implemented in step 1"

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term = 
  failwith "implemented in step 3"

let withFreshVariables (clause:Clause) : Clause =
  failwith "implemented in step 3"

let query (program:list<Clause>) (query:Term) =
  failwith "implemented in step 3"

let rec solve program subst goals = 
  match goals with 
  | g::goals -> 
      // Find all matching clauses for the goal and iterate over them
      let matches = query program g
      for clause, newSubst in matches do
        // Add the body of the clause to the goals and apply the substitution
        let newGoals = clause.Body @ substituteTerms (Map.ofList newSubst) goals
        // Apply the substitution to the existing substitution and append them
        let newSubst = substituteSubst (Map.ofList newSubst) subst @ newSubst
        // Solve the new goals with the new substitution
        solve program newSubst newGoals
  | [] -> 
    // Print the final substitution
    printfn "Solution:"
    for var, term in subst do
      printfn "  %s = %s" var (formatTerm term)

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

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

// Query: father(X, William)
// Result #1: [ X -> Charles, ... ]
solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ]

// Query: father(X, Y)
// Result #1: [ X -> Charles, Y -> William, ... ]
// Result #2: [ X -> George, Y -> William, ... ]
solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ]
