open Dom 

(************************ Initialisation des configurations du jeu ********************)
let grid1 () = 
    let oc = open_out "grid1.txt" in  
        Printf.fprintf oc "A3V00\n";   
        Printf.fprintf oc "B3V03\n";
        Printf.fprintf oc "C2H21\n";
        Printf.fprintf oc "D2V32\n";
        Printf.fprintf oc "E3H33\n";
        Printf.fprintf oc "F3H52\n";
        Printf.fprintf oc "G2V45\n";
        close_out oc
let grid2 () = 
    let oc = open_out "grid2.txt" in  
        Printf.fprintf oc "A3H03\n";   
        Printf.fprintf oc "B2V32\n";
        Printf.fprintf oc "C2H20\n";
        Printf.fprintf oc "D3V24\n";
        Printf.fprintf oc "E2H53\n";
        close_out oc
let grid3 () = 
    let oc = open_out "grid3.txt" in  
        Printf.fprintf oc "A3V35\n";   
        Printf.fprintf oc "B3V23\n";
        Printf.fprintf oc "C2H21\n";
        Printf.fprintf oc "D2H31\n";
        Printf.fprintf oc "E2H52\n";
        Printf.fprintf oc "F2V41\n";
        close_out oc
let grid4 () = 
    let oc = open_out "grid4.txt" in  
        Printf.fprintf oc "A2V02\n";   
        Printf.fprintf oc "B2H03\n";
        Printf.fprintf oc "C2H22\n";
        Printf.fprintf oc "D2V20\n";
        Printf.fprintf oc "E2V11\n";
        Printf.fprintf oc "F2V24\n";
        Printf.fprintf oc "G2V44\n";
        Printf.fprintf oc "H2H31\n";
        Printf.fprintf oc "I3H40\n";
        Printf.fprintf oc "J2H50\n";
        close_out oc
(******************************************************************************************)  

(* Variable globale contanat les noms des fichiers pour la generation aleatoire *)
let config = [|"grid1.txt"; "grid2.txt";"grid3.txt";"grid4.txt";|]

(*Variable globale contenant une reférence vers le nom du fichier courant *)
let fname = ref "grid1.txt"  

(* variable globale contant une référence vers le bateau sélectionné*)
let current_boat = 
    let (b:Port.boat) = {id = '~'; longueur = 0; orientation = H; x = 0; y=0} in 
        ref b

(* variable globale contant une référence vers l'état actuelle*)
let st = 
    let (s:Port.state) = [|[||]|]in 
        ref s

(* variable globale contant une référence vers le nombre de deplacements effectués*)        
let coups= ref 0

(* fonction executée quand un bateau est selectionné  _OnClick_ : mettre à jour current_boat *)
let select i j () = (current_boat := !st.(i).(j)) 

(* Reinitialiser le jeu avec la meme configuration *)
let reset () =
    current_boat := {id = '~'; longueur = 0; orientation = H; x = 0; y=0};
    coups := 0;
    let file = open_in !fname in 
      st := Port.input_state file;
       Array.iteri (fun i ligne -> 
            Array.iteri (fun j (c:Port.boat) -> 
                    let case1= string_of_int (i) ^ string_of_int (j) in 
                        let c1 = (Document.get_element_by_id document case1 ) in 
                            if  c.id != '~' then (
                                Element.set_attribute c1 "class" (Char.escaped c.id);
                                Element.set_onclick c1 (select i j)
                            )else (
                                Element.set_attribute c1 "class" "";
                                Element.set_onclick c1 (fun () -> ())
                )
                
            ) ligne
         )!st 

(* Mettre à jour la grille à chaque nouveau deplacement et vérifier si le joueur a gagné *)
let update () = 
    Array.iteri (fun i ligne -> 
            Array.iteri (fun j (c:Port.boat) -> 
                    let case1= string_of_int (i) ^ string_of_int (j) in 
                        let c1 = (Document.get_element_by_id document case1 ) in
                        if  c.id != '~' then (
                        Element.set_attribute c1 "class" (Char.escaped c.id);
                        Element.set_onclick c1 (select i j)
                )else (
                    Element.set_attribute c1 "class" "";
                    Element.set_onclick c1 (fun () -> ())
                )
            ) ligne
         )!st;
        let t = (Document.get_element_by_id document "gagne" ) in 
        if (Moves.win !st) then (
            Element.set_text_content t ("Felicitation! Vous avez gagne en "^(string_of_int !coups)^" coups.");
           
        ) else (
           Element.set_text_content t ""
        )

(* Avance le current_boat et met à jour l'état  *)
let avancer () = 
    let car = Char.escaped (!current_boat.id) in
        let (m:Moves.move)= A !current_boat in 
        st := Moves.apply_move m !st;
        current_boat := (Moves.find_boat car.[0] !st) ;
        (*current_boat := {id = '~'; longueur = 0; orientation = H; x = 0; y=0};*)
        coups := !coups +1 ; 
        update ()

(* Recule le current_boat et met à jour l'état  *)
let reculer () = 
    let car = Char.escaped (!current_boat.id) in
        let (m:Moves.move)= R !current_boat in 
        st := Moves.apply_move m !st;
        current_boat := (Moves.find_boat car.[0] !st) ;
        (*current_boat := {id = '~'; longueur = 0; orientation = H; x = 0; y=0};*)
        coups := !coups +1 ; 
        update ()


(* Resoudre le jeu *)
let resoudre () = 
    reset ();
    let sol = Solver.solve_state !st in 
    for i=0 to (String.length sol) -1 do (
         if(sol.[i] != '>' && sol.[i] != '<') then (
         current_boat := (Moves.find_boat sol.[i] !st) ;
         let (m:Moves.move) = match sol.[i+1] with 
            '>' -> A !current_boat | '<'-> R !current_boat in
            st := Moves.apply_move m !st;
            coups := !coups +1 
         )  
    ) done; 
    update ()

(* Generation aleatoire des grilles *)
let random () = 
    let i = Random. int 4  in
    fname := config.(i); 
    reset()

(* Initialise le jeu *)
let set () = 
    let file = open_in !fname in 
      st := Port.input_state file;

      let b1 = (Document.get_element_by_id document "avancer") 
      and b2 = (Document.get_element_by_id document "reculer") 
      and b3 = (Document.get_element_by_id document "reset") 
      and b4 = (Document.get_element_by_id document "resoudre") 
      and b5 = (Document.get_element_by_id document "random") 
      in 
       Element.set_onclick b1 avancer ;
       Element.set_onclick b2 reculer ;
       Element.set_onclick b3 reset ;
       Element.set_onclick b4 resoudre ;
       Element.set_onclick b5 random ;
      (* Représentation des bateaux*)
         Array.iteri (fun i ligne -> 
            Array.iteri (fun j (c:Port.boat) -> 
                if  c.id != '~' then (
                    let case1= string_of_int (i) ^ string_of_int (j) in 
                        let c1 = (Document.get_element_by_id document case1 ) in
                        Element.set_attribute c1 "class" (Char.escaped c.id);
                        Element.set_onclick c1 (select i j)
                )
            ) ligne
         )!st


let play () = 
    grid1();
    grid2();
    grid3();
    grid4();
    
    set ()
     
    
let _ = play ()