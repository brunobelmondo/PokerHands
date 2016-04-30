namespace PokerHands

open Parser
open PokerHand
open Scoring

module Main=
    let computeScoreFromText = parseHand >> computeScore

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

