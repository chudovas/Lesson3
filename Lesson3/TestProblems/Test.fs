open NUnit.Framework 
open FsUnit
open Problems
open System

[<Test>]
let ``test tree`` () =
    mapTree (Node(10, (Node(7, Node(5, Empty, Empty), Node(8, Empty, Empty))), (Node(21, Node(20, Empty, Empty), Node(25, Empty, Empty))))) (fun i -> i + 5)
    |> should equal (Node(15, (Node(12, Node(10, Empty, Empty), Node(13, Empty, Empty))), (Node(26, Node(25, Empty, Empty), Node(30, Empty, Empty))))) 

[<Test>]
let ``test of arifmetic 1`` () =
    eval(Sum(Subtraction(Num(5), Num(4)), Multiplication(Num(1), Negative(Num(1)))))
    |> should equal 0

[<Test>]
let ``test of arifmetic 2`` () =
    (fun() -> eval(Division(Num(666), Num(0))) |> ignore) |> should throw typeof<DivideByZeroException> 

[<Test>]
let ``test of arifmetic 3`` () =
    eval(Multiplication(Subtraction(Num(3), Num(5)), Negative(Num(2)))) |> should equal 4

[<Test>]
let ``test of prime numbers`` () =
    (Seq.toList (Seq.take 20 seqOfPrime)) |> should equal [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71]

let findEven n =
    match n with
    | 0 -> findEvenFilter
    | 1 -> findEvenFold
    | 2 -> findEvenMap

[<Test>]
let ``test of find even 1`` () =
    for i in 0..2 do
        [1; 2; -3; 4; -5; 6; 7] |> findEven i |> should equal 3

[<Test>]
let ``test of find even 2`` () =
    for i in 0..2 do
        [1] |> findEven i |> should equal 0

[<Test>]
let ``test of find even 3`` () =
    for i in 0..2 do
        [] |> findEven i |> should equal 0

[<EntryPoint>]
let main argv =
    ``test tree`` ()
    ``test of arifmetic 1`` ()
    ``test of arifmetic 2`` ()
    ``test of arifmetic 3`` ()
    ``test of prime numbers`` ()
    ``test of find even 1`` ()
    ``test of find even 2``()
    ``test of find even 3`` ()
    1
