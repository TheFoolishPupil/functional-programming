type Name = string;;
type Tlf = string;;
type YearOfBirth = int;;
type Interests = string list;;
type Description = (Tlf * YearOfBirth * Interests);;
type Register = (Name * Description) list;;
type Arrangement = Description -> bool;;

let reg : Register = [("Antonio Banderaz", ("21123443", 1990, ["soccer"; "jazz"]));
                      ("Smokey", ("99999999", 1991, ["football"; "jazz"]));
                      ("Thoams", ("11111111", 1989, ["football"; "hockey"]));
                      ("Carlos Danger", ("69696969", 1980, ["soccer"; "jazz"]))]

let p1 (des : Description) = 
  match des with
  | (_, yob, xs) when yob > 1982 && List.contains "soccer" xs && List.contains "jazz" xs -> true
  | _ -> false

let p2 (des : Description) =
  match des with
  | (_, yob, xs) when yob > 1982 && (List.contains "soccer" xs || List.contains "jazz" xs) -> true
  | _ -> false

// Arrangement -> Register -> List<Name * Tlf>
let rec extractTargetGroup (p : Arrangement) (reg: Register) =
  match reg with
  | [] -> []
  | (name, (tlf, yob, is))::rs when p (tlf, yob, is) -> (name, tlf)::extractTargetGroup p rs
  | _::rs -> extractTargetGroup p rs
