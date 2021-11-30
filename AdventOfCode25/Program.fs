let MODULO = 20201227uL
let doRSARound subject cur =
    (cur * subject) % MODULO
    
let calculateRounds subject target =
    let mutable cur = 1uL
    let mutable rounds = 0
    while cur <> target do
        cur <- doRSARound subject cur
        rounds <- rounds + 1
    rounds
    
let doNRounds subject rounds =
    let mutable cur = 1uL
    for i = 1 to rounds do
        cur <- doRSARound subject cur
    cur
    
let calculateEncryptionKey subject keyPub doorPub  =
    let keyRounds = calculateRounds subject keyPub
//    let doorRounds = calculateRounds subject doorPub
    let result = doNRounds doorPub keyRounds
    result

[<EntryPoint>]
let main argv =    
    // test case
    assert((calculateEncryptionKey 7uL 5764801uL 17807724uL) = 14897079uL)
    
    let result = calculateEncryptionKey 7uL 10705932uL 12301431uL // 7 is a guess
    printfn "Calculated: %d" result
    0