let findEvenMap list =
    list |> List.map (fun i -> ((abs i) + 1) % 2) |> List.sum 

let findEvenFilter list =
    list |> List.filter (fun i -> (abs i) % 2 = 0) |> List.length

let findEvenFold = 
    List.fold (fun acc i -> acc + ((abs i) + 1) % 2) 0

type Tree<'a> = 
    | Node of 'a * Tree<'a> * Tree<'a> 
    | Empty

let rec mapTree tree func =
    match tree with
    | Empty -> tree
    | Node(v, left, right) -> Node(func v, mapTree left func, mapTree right func)
    
type ArifmeticalProp = 
    | Num of int 
    | Sum of ArifmeticalProp * ArifmeticalProp 
    | Subtraction of ArifmeticalProp * ArifmeticalProp 
    | Multiplication of ArifmeticalProp * ArifmeticalProp
    | Division of ArifmeticalProp * ArifmeticalProp
    | Negative of ArifmeticalProp

let rec eval (p: ArifmeticalProp) = 
    match p with 
    | Num(a) -> a 
    | Sum(a, b) -> eval a + eval b 
    | Subtraction(a, b) -> eval a - eval b 
    | Multiplication(a, b) -> eval a * eval b
    | Division(a, b) -> eval a / eval b
    | Negative(a) -> -(eval a)


let seqOfPrime = 
    let isPrime (primeNums : seq<int>) num =
        primeNums |> Seq.takeWhile (fun x -> x * x <= num) |> Seq.map (fun x -> not (num % x = 0)) |> Seq.fold (fun acc x -> acc && x) true

    let rec seqOfPrimeRec (primeNums : seq<int>) (nums : seq<int>) =    
        seq {
            if (isPrime primeNums (Seq.head nums)) 
            then 
                yield (Seq.head nums)
                yield! seqOfPrimeRec (Seq.append primeNums [Seq.head nums]) (Seq.tail nums) 
            else 
                yield! seqOfPrimeRec primeNums (Seq.tail nums)
        }
    seqOfPrimeRec Seq.empty (Seq.initInfinite (fun x -> x + 2))

[<EntryPoint>]
let main argv =
    0
    