
module SeqChop = 

    // sub sequences are captured as a list
    // is a safe alternate since the caller cannot affect the seq of seq
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


    // uses a mutable to capture the state of the sequence
    // does not need to allocate a list but if the caller accesses the seq of seq, out of order, 
    // the result is indeterminable 
    let SubSequence size sequence = 

        let mutable sq = sequence

        let rec subSequence sz =
            seq {
                match not <| Seq.isEmpty sq, sz with 
                |   false, _    
                |   true, 0     ->  ()   
                |   true, _     ->  yield   (Seq.head sq)
                                    sq <- Seq.skip 1 sq
                                    yield!  subSequence (sz - 1) 
            } 

        let rec subSequenceMain () =
            seq {
                match not <| Seq.isEmpty sq with 
                |   true    ->  yield   subSequence size        // seq
                                yield!  subSequenceMain ()      // seq of seq
                |   false   ->  ()
            } 
        subSequenceMain ()



[<EntryPoint>]
let main argv = 
    
    let x = [|1; 2; 3; 4; 5; 6; 7|]
    
    let y = SeqChop.SubSequence 3 x

    y 
    |>  Seq.iter
            (fun sub -> 
                sub |> Seq.iter (printf "%A")
                printfn "") 

    0 // return an integer exit code
