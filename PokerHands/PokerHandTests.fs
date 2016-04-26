module PokerHands.Tests

open System
open NUnit.Framework
open PokerHand

[<Test>]
let ``should Find High Card when nothing special`` () = 
    let obtained = computeScoreFromText "2D 3H 4C 7S AD"
    Assert.AreEqual(HighCard, obtained)

[<Test>]
let ``should Find Pair when two cards have the same value`` () = 
    let obtained = computeScoreFromText "2D 3H 4C AS AD"
    Assert.AreEqual(Pair, obtained)

[<Test>]
let ``should Find Two Pairs when two cards have the same value two times`` () = 
    let obtained = computeScoreFromText "2D 4H 4C AS AD"
    Assert.AreEqual(TwoPairs, obtained)

[<Test>]
let ``should Find Three of a kind when three cards have the same value`` () = 
    let obtained = computeScoreFromText "2D 3H AC AS AD"
    Assert.AreEqual(ThreeOfAKind, obtained)

[<Test>]
let ``should Find four of a kind when four cards have the same value`` () = 
    let obtained = computeScoreFromText "2D AH AC AS AD"
    Assert.AreEqual(FourOfAKind, obtained)

[<Test>]
let ``should Find full house when three cards have same value and two other are a pair`` () = 
    let obtained = computeScoreFromText "2D 2H AC AS AD"
    Assert.AreEqual(FullHouse, obtained)

[<Test>]
let ``should Find flush when all cards have the same suit`` () = 
    let obtained = computeScoreFromText "2D 3D 6D TD AD"
    Assert.AreEqual(Flush, obtained)

[<Test>]
let ``should Find straight when all cards follow each other`` () = 
    let obtained = computeScoreFromText "2D 3D 4H 5D 6D"
    Assert.AreEqual(Straight, obtained)

[<Test>]
let ``should Find straight flush when all cards follow each other and have the same suit`` () = 
    let obtained = computeScoreFromText "2D 3D 4D 5D 6D"
    Assert.AreEqual(StraightFlush, obtained)
