
module SeqChop = 

    let Seqment segmentSize source = 
        let rec seqment size acc src = 
            seq {
                match Seq.isEmpty src, size with 
                |   true, _     ->  yield (List.rev acc |> List.toSeq)
                |   _, 0        ->  yield (List.rev acc |> List.toSeq)
                                    yield! seqment segmentSize [] src
                |   _           ->  yield! seqment (size - 1) (Seq.head src :: acc) (Seq.skip 1 src) 
            }   
        seqment segmentSize [] source 


[<EntryPoint>]
let main argv = 
    
    let x = [|1; 2; 3; 4; 5; 6; 7|]
    
    let y = SeqChop.Seqment 3 x

    y |>
    Seq.iter
        (printfn "%A") 

    0 // return an integer exit code
