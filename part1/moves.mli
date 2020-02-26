type move = A of Port.boat | R of Port.boat
exception Cannot_move
(* convertit un move en chaine de caracteres *)
val string_of_move : move -> string
(* convertit une chaine de caracteres en move *)
val move_of_string : string -> move

(* apply_move m s applique le mouvement m depuis l’etat s et retourne
le nouvel etat. Leve l’ exception Cannot_move si le mouvement est
impossible *)
val apply_move : move -> Port.state -> Port.state
(* indique si un etat est une position gagnante *)
val win : Port.state -> bool
(* verifie si une suite de mouvements depuis l’etat en parametre est
une solution *)
val check_solution : Port.state -> string -> bool