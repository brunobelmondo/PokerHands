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

    let getValue card = card.Value
    let getNumericValue card = int card.Value
    let getSuit card = card.Suit


