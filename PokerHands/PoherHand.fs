namespace PokerHands

module PokerHand =
    type Suit = |Heart
                |Diamond
                |Spade
                |Clubs

    type Value =  |Two
                  |Three
                  |Four
                  |Five
                  |Six
                  |Seven
                  |Height
                  |Nine
                  |Ten
                  |Jack
                  |Queen
                  |King
                  |Ace

    type Card = {Value:Value; Suit:Suit}
     
    let parseSuit char =
        match char with
        | 'H' -> Heart
        | 'D' -> Diamond
        | 'S' -> Spade
        | 'C' -> Clubs

     
    let parseValue char = 
        match char with
        |'2'-> Two
        |'3'-> Three
        |'4'-> Four
        |'5'-> Five
        |'6'-> Six
        |'7'-> Seven
        |'8'-> Height
        |'9'-> Nine
        |'T'-> Ten
        |'J'-> Jack
        |'Q'-> Queen
        |'K'-> King
        |'A'-> Ace


    let getValue card = card.Value

    let parseCard valueToParse suitToParse =
        let value = parseValue valueToParse
        let suit = parseSuit suitToParse
        {Value=value;Suit=suit} 

    let parseCardText (token:string) = 
        match Seq.toList token with
        | v::s::[] -> parseCard v s

    type Hand = {Cards :Card list}

    let parseHand (text:string) = 
        let cards = Array.toList (text.Split(' ')) 
                    |> List.map parseCardText
        {Cards=cards}

    type Score = |HighCard
                 |Pair
                 |TwoPairs
                 |ThreeOfAKind
                 |Straight
                 |Flush
                 |FullHouse
                 |FourOfAKind
                 |StraightFlush

    let groupCardsByValue hand = Seq.groupBy getValue hand.Cards

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

    let computeScore hand = 
        match hand with
        | IsFourOfAKind -> FourOfAKind
        | IsThreeOfAKind -> ThreeOfAKind
        | IsTwoPairs -> TwoPairs
        | IsPair -> Pair
        | _ -> HighCard


    let computeScoreFromText = parseHand >> computeScore
