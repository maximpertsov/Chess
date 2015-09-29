namespace Chess
open System
open NUnit.Framework

[<TestFixture>]
module BoardTests =

    module CB = Chess.Board
    module CP = Chess.Piece

    // Chess pieces
    let black_bishop = CP.Black, CP.Bishop
    let white_bishop = CP.White, CP.Bishop  
          
    let black_rook = CP.Black, CP.Rook

    let black_king = CP.Black, CP.King

    let white_pawn = CP.White, CP.Pawn
    let black_pawn = CP.Black, CP.Pawn

    // Parametrized empty board test
    let emptyBoardTest n =
        Assert.AreEqual(Chess.Board.empty n, [| for _ in 1..n -> [| for _ in 1..n -> None|] |])

    [<Test>]
    let ``4x4 EmptyBoard Is A 4 x 4 Element Array of Nones``() =
        List.iter emptyBoardTest [1 .. 10]

    [<Test>]
    let ``Show function correctly generates column names at the bottom``() =
        Assert.AreEqual((CB.show CB.standard).Split('\n').[8], "   Ａ Ｂ Ｃ Ｄ Ｅ Ｆ Ｇ Ｈ ")

    [<Test>]
    let ``Piece stops moving once it reached end of board``() =
        let b  = CB.empty 8
        Assert.AreEqual(CB.moveHelper black_rook 3 2 b (1,0), Set.ofList [for i in 4..8 -> i,2])

    [<Test>]
    let ``Piece stops moving before it reaches another piece of the same color``() =
        let b = CB.empty 8 |> CB.setPiece black_king 3 5
        Assert.AreEqual(CB.moveHelper black_rook 3 2 b (0,1), Set.ofList [(3,3);(3,4)])    

    [<Test>]
    let ``Rook moves freely in orthogonal directions and stops before it reaches an ally piece``() =
        let b = CB.empty 8 |> CB.setPiece black_king 3 5 |> CB.setPiece black_pawn 1 2
        Assert.AreEqual(CB.legalMoves black_rook 3 2 b, Set.ofList ([for j in 1..4 -> 3,j]@[for i in 2..8 -> i,2]) |> Set.remove (3,2))

    [<Test>]
    let ``Piece stops moving when it reaches another piece of the opposite color``() =
        let b = CB.empty 8 |> CB.setPiece white_bishop 2 2
        Assert.AreEqual(CB.moveHelper black_bishop 5 5 b (-1,-1), Set.ofList [for i in 2..4 -> i,i]) 

    [<Test>]
    let ``Bishop moves freely in diagonal directions and stops when it reaches an enemy piece``() =
        let b = CB.empty 8 |> CB.setPiece white_bishop 2 2 |> CB.setPiece white_pawn 3 7
        Assert.AreEqual(CB.legalMoves black_bishop 5 5 b, Set.ofList ([for i in 2..8 -> i,i]@[for i in 3..8 -> i,8-i+2]) |> Set.remove (5,5))