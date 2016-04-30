namespace PokerHands

module PokerHand =
    type Suit = |Heart
                |Diamond
                |Spade
                |Clubs

    type Value =  |Two = 2
                  |Three = 3
                  |Four = 4
                  |Five = 5
                  |Six = 6
                  |Seven = 7
                  |Height = 8
                  |Nine = 9
                  |Ten = 10
                  |Jack = 11
                  |Queen = 12
                  |King = 13
                  |Ace = 14

    type Card = {Value:Value; Suit:Suit}
     
    type Hand = {Cards :Card list}

    type Player = {Name: string; Hand: Hand}

    let parseSuit char =
        match char with
        | 'H' -> Heart
        | 'D' -> Diamond
        | 'S' -> Spade
        | 'C' -> Clubs

     
    let parseValue char = 
        match char with
        |'2'-> Value.Two
        |'3'-> Value.Three
        |'4'-> Value.Four
        |'5'-> Value.Five
        |'6'-> Value.Six
        |'7'-> Value.Seven
        |'8'-> Value.Height
        |'9'-> Value.Nine
        |'T'-> Value.Ten
        |'J'-> Value.Jack
        |'Q'-> Value.Queen
        |'K'-> Value.King
        |'A'-> Value.Ace


    let getValue card = card.Value
    let getNumericValue card = int card.Value
    let getSuit card = card.Suit

    let parseCard valueToParse suitToParse =
        let value = parseValue valueToParse
        let suit = parseSuit suitToParse
        {Value=value;Suit=suit} 

    let parseCardText (token:string) = 
        match Seq.toList token with
        | v::s::[] -> parseCard v s

    let isNotEmpty (text:string) = text.Trim().Length > 0

    let parseHand (text:string) = 
        let cards = Array.toList (text.Split(' '))
                    |> List.filter isNotEmpty
                    |> List.map parseCardText
        {Cards=cards}

    let parsePlayer (input:string) =
         let tokens = input.Split(':')
         let name = string (tokens.GetValue(0))
         let handInput = string (tokens.GetValue(1))
         let hand = parseHand handInput
         {Name=name;Hand=hand}

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

    let computeScoreFromText = parseHand >> computeScore

    type ScoredPlayer = {Name:string;Score:Score;Hand:Hand}

    let scorePlayer (player:Player) = { 
                                Name = player.Name;
                                Score = computeScore player.Hand;
                                Hand = player.Hand
                             }
    let parseAndComputeScore = parsePlayer >> scorePlayer

    let printScore score =
        match score with
        |Score.HighCard -> "high card"
        |Score.Pair -> "pair"
        |Score.TwoPairs -> "two pairs"
        |Score.ThreeOfAKind  -> "three of a kind"
        |Score.Straight -> "straight"
        |Score.Flush -> "flush"
        |Score.FullHouse -> "full house"
        |Score.FourOfAKind -> "four of a kind"
        |Score.StraightFlush -> "straight flush"


    let findWinner (input:string) = 
        let playersInput = input.Split('\t')
        let player1 = parseAndComputeScore (string (playersInput.GetValue(0)))
        let player2 = parseAndComputeScore (string (playersInput.GetValue(1)))
        match player1, player2 with
        | p1, p2 when p1.Score > p2.Score -> p1.Name + " wins. - with " + (printScore p1.Score)
        | p1, p2 when p2.Score > p1.Score -> p2.Name + " wins. - with " + (printScore p2.Score)
        | _,_ -> "Tie."
