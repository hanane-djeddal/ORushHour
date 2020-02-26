exception FileVide (* Pour la structure de la file lors de l'explorations des solution *) 
exception Fin  (*Pour indiquer la fin de la recherche : solution trouvée *)

let marked = ref [""] (*Stocker les états visités sous forme d'une chaine *)

(* tester si un état a été déjà visité *)
let isMarked (s:Port.state) =
  let chaine =ref "" in 
  (* transformer s en une chaine de caractères en concaténant les id des bateaux *)
  Array.iter (fun  ligne -> 
    Array.iter (fun  (b:Port.boat) -> 
      chaine :=!chaine ^ (Char.escaped b.id)) ligne 
    ) s;
    (*tester si la chaine est dans marked *)
    if List.mem !chaine !marked then true
    else (marked := (!chaine)::!marked; false)

    (*traitement de la file *)
let enfiler f s = if (isMarked (fst s)) then f else List.append f [s]
let defiler f = match f with 
  []->raise FileVide
  |hd:: l->hd,l

(* Port.state -> Moves.move list *)
let all_possible_moves (stat:Port.state)=
  let h = Array.length stat and w= Array.length stat.(0) in 
  let mref=ref [] in (* va contenir les moves possibles *)
  (*parcourir state, tester les cases, si c'est un bateau recupérer les moves possibles *)
    Array.iteri (fun i ligne -> 
      Array.iteri (fun j (b:Port.boat) ->
      (*c'est un bateau *)
        if(b.id != '~' && b.x = i && b.y =j) then (
          (*on teste les cases selon son orientation pour savoir si on peut déplacer *)
          match b.orientation with 
          H-> if (j - 1 ) >= 0 then (
                if(stat.(i).(j-1).id ='~') then( (*case vide donc move possible *)
                let (m:Moves.move)= R (stat.(i).(j)) in 
                 mref := List.append !mref [m] ) );
              if (j+ b.longueur ) < w then ( 
                  if(stat.(i).(j+b.longueur).id ='~') then( (*case vide donc move possible *)
                  let (m:Moves.move)= A (stat.(i).(j)) in
                   mref := List.append !mref [m] ) )
          |V->if (i - 1 ) >= 0 then ( 
                if(stat.(i-1).(j).id ='~') then( (*case vide donc move possible *)
                let (m:Moves.move)= R (stat.(i).(j)) in 
                  mref := List.append !mref [m] ) );
              if (i+ b.longueur ) < h then (
                if(stat.(i+b.longueur).(j).id ='~') then( (*case vide donc move possible *)
                let (m:Moves.move)= A (stat.(i).(j)) in
                mref := List.append !mref [m] ) )
        )
      ) ligne
    ) stat;
  !mref

(* Port.state -> Moves.move list -> Port.state list *)
let all_reachable_states (stat:Port.state) (m:Moves.move list)=
  let sref = ref [] in (*va contenir les états *)
  List.iter(fun mov -> 
    sref := List.append !sref [Moves.apply_move mov stat]
  ) m; 
!sref

(* Port.state -> string *)
let solve_state (stat:Port.state) =
  (*file contient l'état et la chaine des moves entainant cet état *)
  let file =ref (enfiler [] (stat, "")) and solution =ref "" in 
  try (
    (*tant que file non vide *)
  let rec loop_princ elem f=
    let moves = all_possible_moves (fst elem) in (*calculer les moves possibles *)
      file:=f;
      let rec loop l mov = match l with (*parcourir les fils *)
      [] -> () (*fin de la liste des fils *)
      |s :: tl ->
      let smov = (snd elem)^(Moves.string_of_move (List.hd mov)) in (*rajouter le nouveau move*)
      if (Moves.win s ) then (*configuration gagnate *)
        (solution := smov; 
        raise Fin)
      else (file := enfiler !file (s,smov); (*reboucler *)
            loop tl (List.tl mov))

    in loop (all_reachable_states (fst elem) moves) (moves); 
    let e,f= defiler (!file) in
    loop_princ (e:(Port.state*string)) (f:(Port.state*string) list) in 
    let e,f= defiler (!file) in
    loop_princ (e:(Port.state*string)) (f:(Port.state*string) list)

  ) with FileVide -> "Pas de Solution" |Fin -> (!solution)

(* in_channel -> string *)
let solve_input inputch =
  let (s:Port.state) = Port.input_state inputch in 
    solve_state s