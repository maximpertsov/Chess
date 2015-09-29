namespace Chess
open System
open NUnit.Framework

[<TestFixture>]
module BoardTests =

    module CB = Chess.Board

    // Parametrized empty board test
    let emptyBoardTest n =
        Assert.AreEqual(Chess.Board.empty n, [| for _ in 1..n -> [| for _ in 1..n -> None|] |])

    [<Test>]
    let ``4x4 EmptyBoard Is A 4 x 4 Element Array of Nones``() =
        List.iter emptyBoardTest [1 .. 8]

    [<Test>]
    let ``Show function correctly generates column names at the bottom``() =
        Assert.AreEqual((CB.show CB.standard).Split('\n').[8], "   Ａ Ｂ Ｃ Ｄ Ｅ Ｆ Ｇ Ｈ ")