
  sealed trait Plec
  case object Kobieta extends Plec
  case object Mezczyzna extends Plec


  sealed trait Status_osoby
  case object Dziecko extends Status_osoby
  case object Uczen extends Status_osoby
  case object Student extends Status_osoby
  case object Pracownik extends Status_osoby
  case object Emeryt extends Status_osoby


def definicja(tuple:(String, String, Plec, Int, String)):Status_osoby = {
  tuple._4 match
    case _ if tuple._5 != "" || (tuple._4>=26 && ((tuple._4 <= 60 && tuple._3 == Kobieta)||(tuple._4 <= 65 && tuple._3 == Mezczyzna))) => Pracownik
    case tuple._4 if tuple._4<7 => Dziecko
    case tuple._4 if tuple._4>=7 && tuple._4<=19 => Uczen
    case tuple._4 if tuple._4>=20 && tuple._4<=25 => Student
    case _ => Emeryt
}

  definicja(("Anna", "Cebula", Kobieta, 55, "LO6"));
  definicja(("Adam", "Buczylko", Mezczyzna, 84, ""))
  definicja(("Jakub", "Cebula", Mezczyzna, 20, ""))
  definicja(("Janina", "Buczylko", Kobieta, 84, "PWr"))