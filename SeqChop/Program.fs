open System

module SeqChop = 

    // sub sequences are captured as a list
    // is a safe alternate since the caller cannot affect the seq of seq
    let SubSequenceSafe segmentSize source = 
        let rec subSequence size acc src = 
            seq {
                match not <| Seq.isEmpty src, size with 
                |   false, _    ->  yield (List.rev acc |> List.toSeq)
                |   _, 0        ->  yield (List.rev acc |> List.toSeq)
                                    yield! subSequence segmentSize [] src
                |   _           ->  yield! subSequence (size - 1) (Seq.head src :: acc) (Seq.skip 1 src) 
            }   
        subSequence segmentSize [] source 


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
    
    let y = SeqChop.SubSequenceSafe 3 x

    y 
    |>  Seq.iter
            (fun sub -> 
                sub |> Seq.iter (printf "%A")
                printfn "") 

    
    Console.WriteLine ()
    Console.WriteLine "Enter to exit ..."        
    Console.ReadLine () |> ignore

    0 // return an integer exit code
