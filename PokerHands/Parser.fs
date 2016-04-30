
namespace PokerHands
open PokerHand

module Parser=

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
         {Player.Name=name;Hand=hand}
