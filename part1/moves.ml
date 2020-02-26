type move = A of Port.boat | R of Port.boat
exception Cannot_move

(* retourne true si on peut occuper les cases (l1,c1) et (l2,c2) false sinon *)
let test_move l1 c1 l2 c2 (s:Port.state) = 
  let h = Array.length s and w= Array.length s.(0) in 
  if (l2 >= h || l1 >= h || c1 >=w || c2>=w 
      ||s.(l2).(c2).id != '~') then (* verifier si s[l2,c2] ne contient pas un bateau *)
    false else true

(* cherche le bateau avec id=sb dans state *)
let find_boat sb (stat:Port.state) =
  let h = Array.length stat and w= Array.length stat.(0) in 
    let rec loop i j= (* parcourir state *)
      if(i = h ) then (let  (* le bateau n'existe pas dans state *)
        (b:Port.boat)={id =sb; longueur = 0; orientation = V; x = 0; y=0} in b )
      else let c= stat.(i).(j) in  (*on trouve le bateau *)
        if (c.id = sb && c.x = i && c.y= j) then (* premier rencontre du bateau, on le retourne *)
          c
        else if (((j+1) mod w) = 0) then loop (i+1) 0
            else loop i (j+1)
    in loop 0 0

(* une copie en profondeur d'une matrice *)
let copie_profondeur a =
  let h = Array.length a and w= Array.length a.(0) in
    let cop = Array.make_matrix h w (a.(0).(0)) in
      Array.iteri (fun i ligne -> cop.(i) <- Array.copy ligne) a; 
  cop

(* move -> string *)
let string_of_move (m:move) =
  match m with
   A b-> (Char.escaped (b.id))^">"
  |R b-> (Char.escaped (b.id))^"<"

(* string -> move *)
let move_of_string s = 
  let (b:Port.boat)= {id ='~'; longueur = 0; orientation = V; x = 0; y=0} in 
  match s.[1] with
   '>'-> let c= {b with id=s.[0]} in A c 
  |'<'-> let c= {b with id=s.[0]} in R c

(* move -> Port.state -> Port.state; s passée en paramètre n'est pas modifier *)
let apply_move m (s:Port.state) =
  let stat = copie_profondeur s in (* trvailler sur une copie *)
  let (vide:Port.boat) = {id = '~'; longueur = 0; orientation = H; x = 0; y=0} in 
  (*Tester si Avancer ou Reculer *)
  (match m with 
  (*Avancer *)
  A b ->( match b.orientation with (*Traiter selon orientation du bateau *)
    (*Avancer Horizontalement *)
    Port.H-> if not (test_move b.x (b.y+1) b.x (b.y+b.longueur) stat) then
      raise Cannot_move 
      (* on effectue le move, en libérant des cases et occupant les nouvelles *)
      else let x1=b.x and y1=b.y in
        let  newb={b with y = y1+1}  in
        stat.(x1).(y1)<-vide; (*liberer *)
        for i=1 to b.longueur do 
          stat.(x1).(y1+i) <- newb (*occuper *) 
        done
    (*Avancer Verticalement *)
    |Port.V ->if not (test_move (b.x+1) b.y (b.x+b.longueur) b.y stat) then
      raise Cannot_move
      else let x1=b.x and y1=b.y in 
        let newb = {b with x =x1+1} in
        stat.(x1).(y1)<-vide;
        for i=1 to b.longueur do 
          stat.(x1+i).(y1) <- newb 
        done )
  (*Reculer *)
  |R b ->( match b.orientation with 
    (*Reculer Horizontalement *)
    Port.H-> if not (test_move b.x (b.y+b.longueur-2) b.x (b.y-1) stat) then
    raise Cannot_move
      else let x1=b.x and y1=b.y in
        let newb = {b with y =y1-1} in 
        stat.(x1).(y1+b.longueur-1)<-vide;
        for i=(-1) to b.longueur-2 do 
          stat.(x1).(y1+i) <- newb 
        done
    (*Reculer Verticalement *)
    |Port.V -> if not (test_move (b.x+b.longueur-2) b.y (b.x-1) b.y stat) then
    raise Cannot_move
      else let x1=b.x and y1=b.y in
        let newb = {b with x =x1-1} in
        stat.(x1+b.longueur-1).(y1)<-vide;
        for i=(-1) to b.longueur-2 do 
          stat.(x1+i).(y1) <- newb 
        done) );
  stat
(* Port.state -> bool *)
(* win si le bateau à la case (2,4), ses coordonnées sont (2,4) et il est Horizontal *)
let win (stat:Port.state) = 
  let b = stat.(2).(4) in
   (b.x = 2 && b.y=4 && b.orientation = H)

(* Port.state -> string -> bool*)
let check_solution (stat:Port.state) s= 
  let sref=(ref stat) in
  let l =String.length s  in 
    let rec loop i = (* parcourir les moves dans la chaine *)
      if (i=l || (i+1)=l) then () else (*fin de la chaine, dernier move *)
      if(s.[i] ='>' || s.[i] ='<') then loop (i+1) (*ignorer les signes < et > , passer au suivant*)
      else match s.[i+1] with (*Si une Lettre, tester le caractère suivant *)
      '>' -> sref := apply_move (A (find_boat s.[i] !sref)) !sref; loop (i+2)
      |'<' -> sref := apply_move (R (find_boat s.[i] !sref)) !sref; loop (i+2) 
  in loop 0;
  win !sref
   

