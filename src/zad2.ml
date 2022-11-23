type samochod = (string * (string * int));;
let listaSamochodow = [("Opel", ("astra",1999)); ("Renault", ("megane", 2004)); ("Opel", ("corsa", 2009)); ("Nissan",( "micra", 2004)); ("Opel", ("corsa", 2009)); ("Nissan", ("micra", 2004))];;


let killingDuplicates xs =
  let rec killingDuplicates_rec list res =
    match list with
      []->res
     |_-> killingDuplicates_rec (List.filter (fun (x,(y,z)) -> (fst(List.hd list)<>x || fst(snd(List.hd list))<>y)) list) ((List.hd list)::res)
  in killingDuplicates_rec xs [];;


let znajdzSamochod list wartosc =
  let rec count xs res =
      if xs=[] then res
      else count (List.tl xs) ((fst(List.hd xs),(fst(snd(List.hd xs)),(List.fold_left (fun y x -> if x = (List.hd xs) then y+1 else y+0) 0 xs)))::res)
    
  in  killingDuplicates (List.rev (count (List.filter (fun x -> snd(snd(x)) = wartosc) list) [])) ;;



znajdzSamochod listaSamochodow 2004;;









let rec fnx list result remaining =
  match list with
    [] when remaining=[] ->result
   |[] when remaining<>[] -> fnx remaining result []
   |(x,(y,z))::t when result=[] -> fnx t ((x,(y,z))::result) []
   |(x,(y,z))::t when fst(List.hd result)=x && fst(snd(List.hd result))=y && snd(snd(List.hd result))>=z -> fnx t result remaining
   |(x,(y,z))::t when fst(List.hd result)=x && fst(snd(List.hd result))=y && snd(snd(List.hd result))<z -> fnx t ((x,(y,z))::(List.tl result)) remaining
   |(x,(y,z))::t when fst(List.hd result)<>x || fst(snd(List.hd result))<>y -> fnx t result ((x,(y,z))::remaining)
   |_ -> fnx (List.tl (List.rev remaining)) ((List.hd (List.rev remaining))::result) [];;
