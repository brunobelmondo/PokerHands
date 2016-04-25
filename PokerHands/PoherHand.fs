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

    let groupCardsByValue Hand = Seq.groupBy getValue Hand.Cards

    let (|IsFourOfAKind|_|) Hand = 
       let cardsGrouped = groupCardsByValue Hand
       match Seq.length cardsGrouped with
       | 2 -> Some IsFourOfAKind
       | _ -> None

    let (|IsThreeOfAKind|_|) Hand = 
       let cardsGrouped = groupCardsByValue Hand
       match Seq.length cardsGrouped with
       | 3 -> Some IsThreeOfAKind
       | _ -> None

    let (|IsTwoPairs|_|) Hand = 
       let cardsGrouped = groupCardsByValue Hand
       match Seq.toList cardsGrouped with
       | (k1,l1)::b::c when l1.length = 2-> Some IsTwoPairs
       | _ -> None

    let (|IsPair|_|) Hand = 
       let cardsGrouped = groupCardsByValue Hand
       match Seq.length cardsGrouped with
       | 4 -> Some IsPair
       | _ -> None

    let computeScore hand = 
        match hand with
        | IsFourOfAKind -> FourOfAKind
        | IsThreeOfAKind -> ThreeOfAKind
        | IsPair -> Pair
        | _ -> HighCard


    let computeScoreFromText = parseHand >> computeScore
