type samochod = (string * (string * int));;
let listaSamochodow = [("Opel", ("astra",1999)); ("Renault", ("megane", 2004)); ("Opel", ("corsa", 2009)); ("Nissan",( "micra", 2004)); ("Opel", ("corsa", 2009)); ("Nissan", ("micra", 2004))];;

let znajdzSamochod list wartosc =
  let rec count xs res ={
      if xs=[]
         else fold_left (fun x -> {if x = (List.hd xs) then _+1 else _+0}) 0 xs
    let rec findInList elem xss acc =
      match xss with
        h::t when h=elem -> findInList elem (List.tl xss) acc+1
      |[]->acc
      |_ -> findInList elem (List.tl xss) acc
        in (fst(List.hd xs),fst(snd(List.hd xs)),findInList (List.hd xs) (List.tl xs) 1)::res
                                                                                            count (List.tl xs) res
    }
  in count (List.filter (fun x -> snd(snd(x)) = wartosc) list) [];;


znajdzSamochod listaSamochodow 2004;;

