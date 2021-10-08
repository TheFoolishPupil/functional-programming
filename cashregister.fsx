type ArticleCode = string;;
type ArticleName = string;;
type Price       = int;;
type Register    = (ArticleCode * (ArticleName * Price)) list;;
type NoPieces    = int;;         // np  where np >= 0
type Item        = NoPieces * ArticleCode;;
type Purchase    = Item list;;
type Info        = NoPieces * ArticleName * Price;;
type Infoseq     = Info list;;
type Bill        = Infoseq * Price;;

let (reg:Register) = [("a1",("cheese",25));
                      ("a2",("herring",4));
                      ("a3",("soft drink",5)) ];;

let pur = [(3,"a2"); (1,"a1")];;


let rec findArticle ac = function
  | (ac',adesc)::_ when ac=ac' -> adesc
  | _::reg -> findArticle ac reg 
  | _ ->failwith(ac + " is an unknown article code");;


let tryFindArticle ac reg =
  let x = List.tryFind (fun (ac',_) -> ac' = ac) reg
  match x with
  | Some (_, adesc) -> adesc
  | None -> failwith(ac + " is an unknown article code")


let rec makeBill reg = function
  | []           -> ([],0)
  | (np,ac)::pur -> let (aname,aprice) = findArticle ac reg
                    let tprice         = np*aprice
                    let (billtl,sumtl) = makeBill reg pur
                    ((np,aname,tprice)::billtl,tprice+sumtl);;

let makeBill' reg pur =
  let f (np,ac) (infos,billprice) =
    let (aname, aprice) = tryFindArticle ac reg
    let tprice          = np*aprice
    ((np,aname,tprice)::infos, tprice+billprice)
  List.foldBack f pur ([],0)