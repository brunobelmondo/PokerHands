namespace PokerHands
open System
open PokerHand

module Scoring = 
    type Score = |HighCard = 0
                 |Pair = 1
                 |TwoPairs = 2
                 |ThreeOfAKind = 3
                 |Straight = 4
                 |Flush = 5
                 |FullHouse = 6
                 |FourOfAKind = 7
                 |StraightFlush = 8

    let groupCardsByValue hand = Seq.groupBy getValue hand.Cards
    let groupCardsBySuit hand = Seq.groupBy getSuit hand.Cards

    let extractGroupOfCardsByCount count hand= 
       let cardsGrouped = groupCardsByValue hand
       Seq.filter (fun (key,values) -> (Seq.length values) = count) cardsGrouped
     
    let extractPairs = extractGroupOfCardsByCount 2
    let extractThrees = extractGroupOfCardsByCount 3
    let extractFours = extractGroupOfCardsByCount 4

    let (|IsFourOfAKind|_|) hand =
       match Seq.length (extractFours hand) with
       | 1 -> Some IsFourOfAKind
       | _ -> None

    let (|IsFullHouse|_|) hand =
       let threeOfAKind = Seq.length (extractThrees hand) = 1
       let pair = Seq.length (extractPairs hand) = 1
       match threeOfAKind, pair with
       | true,true -> Some IsFullHouse
       | _,_ -> None

    let (|IsThreeOfAKind|_|) hand = 
       match Seq.length (extractThrees hand) with
       | 1 -> Some IsThreeOfAKind
       | _ -> None

    let (|IsTwoPairs|_|) hand = 
       match Seq.length (extractPairs hand) with
       | 2 -> Some IsTwoPairs
       | _ -> None

    let (|IsPair|_|) hand = 
       match Seq.length (extractPairs hand) with
       | 1 -> Some IsPair
       | _ -> None

    let (|IsFlush|_|) hand = 
       let suits = groupCardsBySuit hand
       match Seq.length (suits) with
       | 1 -> Some IsFlush
       | _ -> None

    type StraightChecker = {Valid:bool;Previous:int}

    let checkItemFollows state item = 
        let valid = state.Valid && item-state.Previous = 1
        {Valid=valid;Previous=item}

    let (|IsStraight|_|) hand =
        let cardsValues = Seq.map getNumericValue hand.Cards
        let firstCard = Seq.head cardsValues
        let checkStraight = cardsValues
                            |> Seq.skip 1
                            |> Seq.fold checkItemFollows {Valid=true;Previous=firstCard}
        if checkStraight.Valid then
            Some IsStraight
        else 
            None                        


    let (|IsStraightFlush|_|) hand =
        let isStraight = match hand with
                         |IsStraight -> true
                         |_->false
        let isFlush = match hand with
                         |IsFlush -> true
                         |_->false
        if isStraight && isFlush then          
            Some IsStraightFlush
        else
            None


    let computeScore hand = 
        match hand with
        | IsStraightFlush -> Score.StraightFlush
        | IsFourOfAKind -> Score.FourOfAKind
        | IsFullHouse -> Score.FullHouse
        | IsFlush -> Score.Flush
        | IsStraight -> Score.Straight
        | IsThreeOfAKind -> Score.ThreeOfAKind
        | IsTwoPairs -> Score.TwoPairs
        | IsPair -> Score.Pair
        | _ -> Score.HighCard

    
    type ScoredPlayer = {Name:string;Score:Score;Hand:Hand}

    let scorePlayer (player:Player) = { 
                                Name = player.Name;
                                Score = computeScore player.Hand;
                                Hand = player.Hand
                             }

