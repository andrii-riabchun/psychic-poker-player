module Utils

module Tuple =
    let map f (a,b) = (f a, f b) 

module Seq =
    let findAndSwap old ``new`` = Seq.map (fun v -> if v = old then ``new`` else v)

    let rec combinations n (seq: 'a seq)=
        let lst = List.ofSeq seq
        match (n, lst) with
        | (0,_) -> [[]]
        | (_,[]) -> []
        | (n,x::xs) ->
        let useX = List.map (fun l -> x::l) (combinations (n-1) xs)
        let noX = combinations n xs
        useX @ noX

    let allCombinations seq = [ for i in 0..(Seq.length seq) do yield! combinations i seq] |> Seq.map Seq.ofList