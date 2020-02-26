type orientationType = H | V
type boat = {id : char; longueur : int; orientation: orientationType; mutable x : int; mutable y :int}
type state = boat array array

let boat_of_string chaine =
  let o= match (chaine.[2]) with
    'H'->H
    |'V'->V in
    let l =int_of_string (Char.escaped chaine.[1] ) and 
    v=int_of_string (Char.escaped chaine.[3]) and 
    w=int_of_string (Char.escaped chaine.[4]) in 
    {id = chaine.[0]; longueur = l; orientation = o; x = v; y=w}

let string_of_boat b = 
  let o = match b.orientation with H->"H" | V->"V" and id = Char.escaped b.id in
    id^(string_of_int b.longueur)^o^(string_of_int b.x)^(string_of_int b.y)

    (* Attention! la fonction modifier s: state passée en argument. *)
let add_boat b (s:state)= 
  let l1 = b.x and c1 = b.y and h = Array.length s and w= Array.length s.(0) in
    let l2,c2 = match b.orientation with 
      H->l1,(c1+(b.longueur)-1) | V-> (l1+(b.longueur)-1),c1 in
    if(l2 >= h || l1 >= h || c1 >=w || c2>=w  (* tester si c'est possible de rajouter *)
      || s.(l1).(c1).id != '~' ||
      s.(l2).(c2).id != '~') then 
      raise (Invalid_argument "Impossible d'ajouter le bateau")
    else (*Affecter le bateau à ses cases dans state *)
      s.(l1).(c1)<-b;
      s.(l2).(c2)<-b;
      if(b.longueur == 3) then (
        match b.orientation with 
        H -> s.(l1).(c1+1) <-b
       |V-> s.(l1+1).(c1) <-b);
  s

let grid_of_state s = 
  let h = Array.length s and w= Array.length s.(0) in 
  let grid = Array.make_matrix h w '~' in
  Array.iteri (fun i ligne -> 
    Array.iteri (fun j b -> 
      grid.(i).(j)<-b.id) ligne 
    ) s;
  grid

  (* une case vide de state contient un bateau : ~0H00 *)
let input_state o =
  let b = {id = '~'; longueur = 0; orientation = H; x = 0; y=0} in 
  let c:state=(Array.make_matrix 6 6 b) in
  let s =ref c in 
  let rec cat_rec () = (* parcourir toutes les lignes du fichier *)
    try
     let line=input_line o in 
      let b = boat_of_string line in 
        s := add_boat b !s ; cat_rec ();
    with End_of_file -> close_in o
  in cat_rec ();
  !s

let output_state s o = 
  let g = grid_of_state s in
  Array.iteri (fun i ligne -> 
    Array.iteri (fun j c -> 
     output_string o ((Char.escaped c)^"|") ) ligne; print_newline() 
  ) g
