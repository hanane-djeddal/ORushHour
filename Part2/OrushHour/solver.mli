(* calcul de tous les mouvements possibles depuis un etat *)
val all_possible_moves : Port.state -> Moves.move list
(* calcul de tous les etats atteignables depuis un etat et une liste de
move *)
val all_reachable_states : Port.state -> Moves.move list -> Port.state list
(* calcul d’une solution a partir d’un etat *)
val solve_state : Port.state -> string
(* calcul d’une solution a partir d’un canal d’entree qui contient une
representation de l’etat initial *)
val solve_input : in_channel -> string