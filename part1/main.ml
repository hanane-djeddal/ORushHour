let main = 
  Printf.printf "===================== O'Rush Solver =====================\n";
  try (
    let fname =Sys.argv.(1) in 
    Printf.printf "    Votre input fichier : %s \n" fname;
    let file = open_in fname in 
    Printf.printf "    La solution trouvee : %s\n" (Solver.solve_input file)
  ) with Invalid_argument ("index out of bounds")->
  Printf.printf "Arguments manquants! Veuillez introduire le nom du fichier.\n"
  ;;
main;;