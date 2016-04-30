module PokerHands.Tests

open System
open NUnit.Framework
open PokerHand

[<Test>]
let ``should Find High Card when nothing special`` () = 
    let obtained = computeScoreFromText "2D 3H 4C 7S AD"
    Assert.AreEqual(Score.HighCard, obtained)

[<Test>]
let ``should Find Pair when two cards have the same value`` () = 
    let obtained = computeScoreFromText "2D 3H 4C AS AD"
    Assert.AreEqual(Score.Pair, obtained)

[<Test>]
let ``should Find Two Pairs when two cards have the same value two times`` () = 
    let obtained = computeScoreFromText "2D 4H 4C AS AD"
    Assert.AreEqual(Score.TwoPairs, obtained)

[<Test>]
let ``should Find Three of a kind when three cards have the same value`` () = 
    let obtained = computeScoreFromText "2D 3H AC AS AD"
    Assert.AreEqual(Score.ThreeOfAKind, obtained)

[<Test>]
let ``should Find four of a kind when four cards have the same value`` () = 
    let obtained = computeScoreFromText "2D AH AC AS AD"
    Assert.AreEqual(Score.FourOfAKind, obtained)

[<Test>]
let ``should Find full house when three cards have same value and two other are a pair`` () = 
    let obtained = computeScoreFromText "2D 2H AC AS AD"
    Assert.AreEqual(Score.FullHouse, obtained)

[<Test>]
let ``should Find flush when all cards have the same suit`` () = 
    let obtained = computeScoreFromText "2D 3D 6D TD AD"
    Assert.AreEqual(Score.Flush, obtained)

[<Test>]
let ``should Find straight when all cards follow each other`` () = 
    let obtained = computeScoreFromText "2D 3D 4H 5D 6D"
    Assert.AreEqual(Score.Straight, obtained)

[<Test>]
let ``should Find straight flush when all cards follow each other and have the same suit`` () = 
    let obtained = computeScoreFromText "2D 3D 4D 5D 6D"
    Assert.AreEqual(Score.StraightFlush, obtained)

[<Test>]
let ``should Find winner when player do not have the same kind of score with flash`` () = 
    let obtained = findWinner "Black: 2H 4S 4C 2D 8H\tWhite: 2S 8S AS QS 3S"
    Assert.AreEqual("White wins. - with flush", obtained)

[<Test>]
let ``should Find winner when player do not have the same kind of score with three of a kind`` () = 
    let obtained = findWinner "Black: 2H 4S 4C 2D 8H\tWhite: 2S 8S AS AD AC"
    Assert.AreEqual("White wins. - with three of a kind", obtained)
