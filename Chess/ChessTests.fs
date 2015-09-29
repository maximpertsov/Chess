namespace Chess
open System
open NUnit.Framework

[<TestFixture>]
module BoardTests =

    module CB = Chess.Board
    module CP = Chess.Piece

    // Parametrized empty board test
    let emptyBoardTest n =
        Assert.AreEqual(Chess.Board.empty n, [| for _ in 1..n -> [| for _ in 1..n -> None|] |])

    [<Test>]
    let ``4x4 EmptyBoard Is A 4 x 4 Element Array of Nones``() =
        List.iter emptyBoardTest [1 .. 8]

    [<Test>]
    let ``Show function correctly generates column names at the bottom``() =
        Assert.AreEqual((CB.show CB.standard).Split('\n').[8], "   Ａ Ｂ Ｃ Ｄ Ｅ Ｆ Ｇ Ｈ ")

    [<Test>]
    let ``Piece stops moving once it reached end of board``() =
        let rook = CP.Black, CP.Rook
        let b = CB.empty 8
        Assert.AreEqual(CB.moveHelper rook 3 2 b (1,0), [(8,2);(7,2);(6,2);(5,2);(4,2)])

    [<Test>]
    let ``Piece stops moving before it reaches another piece of the same color``() =
        let black_rook = CP.Black, CP.Rook
        let black_king = CP.Black, CP.King
        let b = CB.empty 8 |> CB.setPiece black_king 3 5
        Assert.AreEqual(CB.moveHelper black_rook 3 2 b (0,1), [(3,4);(3,3)])    

    [<Test>]
    let ``Piece stops moving when it reaches another piece of the opposite color``() =
        let black_bishop = CP.Black, CP.Rook
        let white_bishop = CP.White, CP.Rook
        let b = CB.empty 8 |> CB.setPiece white_bishop 1 1
        Assert.AreEqual(CB.moveHelper black_bishop 5 5 b (-1,-1), [(1,1);(2,2);(3,3);(4,4)])   