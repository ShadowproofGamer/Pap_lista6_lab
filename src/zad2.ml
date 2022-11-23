type samochod = (string * (string * int));;
let listaSamochodow = [("Opel", ("astra",1999)); ("Renault", ("megane", 2004)); ("Opel", ("corsa", 2009)); ("Nissan",( "micra", 2004)); ("Opel", ("corsa", 2009)); ("Nissan", ("micra", 2004))];;





let znajdzSamochod list wartosc =
  let rec killingDuplicates_rec list res =
    match list with
      []->res
     |_-> killingDuplicates_rec (List.filter (fun (x,(y,z)) -> (fst(List.hd list)<>x || fst(snd(List.hd list))<>y)) list) ((List.hd list)::res)
  in let rec count xs res =
      if xs=[] then res
      else count (List.tl xs) ((fst(List.hd xs),(fst(snd(List.hd xs)),(List.fold_left (fun y x -> if x = (List.hd xs) then y+1 else y+0) 0 xs)))::res)
    
  in killingDuplicates_rec (List.rev (count (List.filter (fun x -> snd(snd(x)) = wartosc) list) [])) [] ;;



znajdzSamochod listaSamochodow 2004;;

