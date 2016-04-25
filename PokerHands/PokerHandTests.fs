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
let ``should Find Three of a kind when three cards have the same value`` () = 
    let obtained = computeScoreFromText "2D 3H AC AS AD"
    Assert.AreEqual(ThreeOfAKind, obtained)

[<Test>]
let ``should Find four of a kind when four cards have the same value`` () = 
    let obtained = computeScoreFromText "2D AH AC AS AD"
    Assert.AreEqual(FourOfAKind, obtained)
