type samochod = string * (string * int);;
let listaSamochodow = [("Opel", ("astra",1999)); ("Renault", ("megane", 2004)); ("Opel", ("corsa", 2009)); ("Nissan",( "micra", 2004)); ("Opel", ("corsa", 2009)); ("Nissan", ("micra", 2004))];;

let znajdzSamochod list wartosc =
  let zsumuj lista wynik =
    let element = (List.hd lista)
    in match element with
         element when (List.filter(fun x -> element=x)=[]) -> ele
       |_ -> (fst(element), (fst(snd(element)), 1))::wynik
      
(List.filter (fun x -> snd(snd(x)) = wartosc) list);;

znajdzSamochod listaSamochodow 2004;;

