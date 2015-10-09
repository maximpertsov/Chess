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

    let black_knight = CP.Black, CP.Knight
          
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
        Assert.AreEqual(CB.moveN b 8 black_rook (3, 2) (1,0), Set.ofList [for i in 4..8 -> i,2])

    [<Test>]
    let ``Piece stops moving before it reaches another piece of the same color``() =
        let b = CB.setPiece (CB.empty 8) black_king (3, 5)
        Assert.AreEqual(CB.moveN b 8 black_rook (3, 2) (0,1), Set.ofList [(3,3);(3,4)])    

    [<Test>]
    let ``Piece stops moving when it reaches another piece of the opposite color``() =
        let b = CB.setPiece (CB.empty 8) white_bishop (2, 2)
        Assert.AreEqual(CB.moveN b 8 black_bishop (5, 5) (-1,-1), Set.ofList [for i in 2..4 -> i,i]) 

    [<Test>]
    let ``Rook moves freely in orthogonal directions and stops before it reaches an ally piece``() =
        let b = CB.setPieces (CB.empty 8) [(black_king, 3, 5);(black_pawn, 1, 2)]
        Assert.AreEqual(CB.legalMoves b black_rook (3,2), Set.ofList ([for j in 1..4 -> 3,j]@[for i in 2..8 -> i,2]) |> Set.remove (3,2))

    [<Test>]
    let ``Bishop moves freely in diagonal directions and stops when it reaches an enemy piece``() =
        let ps = [(white_bishop, 2, 2);(white_pawn, 3, 7)]
        let b = CB.setPieces (CB.empty 8) ps
        Assert.AreEqual(CB.legalMoves b black_bishop (5,5), Set.ofList ([for i in 2..8 -> i,i]@[for i in 3..8 -> i,8-i+2]) |> Set.remove (5,5))

    [<Test>]
    let ``White pawn can move up one space or capture the piece diagonally in above it``() =
        let b =  CB.setPiece (CB.empty 8) black_bishop (4, 4)
        Assert.AreEqual(CB.legalMoves b white_pawn (3,3), Set.ofList [(4,3);(4,4)])

    [<Test>]
    let ``Black pawn can move down one space or capture the piece diagonally below it``() =
        let b = CB.setPiece (CB.empty 8) white_bishop (2, 4)
        Assert.AreEqual(CB.legalMoves b black_pawn (3,5), Set.ofList [(2,4);(2,5)])

    [<Test>]
    let ``White pawn can move up two spaces if in 2nd rank from bottom``() =
        let b =  CB.empty 8
        Assert.AreEqual(CB.legalMoves b white_pawn (2,3), Set.ofList [(3,3);(4,3)])

    [<Test>]
    let ``Black pawn can move up two spaces if in 2nd rank from top``() =
        let b =  CB.empty 8
        Assert.AreEqual(CB.legalMoves b black_pawn (7,3), Set.ofList [(6,3);(5,3)])

    [<Test>]
    let ``Pawn cannot capture piece two spaces above it when moving from initial position``() =
        let b =  CB.setPiece (CB.empty 8) white_pawn (5,3) 
        Assert.AreEqual(CB.legalMoves b black_pawn (7,3), Set.ofList [(6,3)])

    [<Test>]
    let ``Black pawn can capture either piece diagonally below it, but not the piece directly below it``() =
        let b = CB.setPieces (CB.empty 8) [for j in 4..6 -> (white_bishop, 2, j)]
        Assert.AreEqual(CB.legalMoves b black_pawn (3,5), Set.ofList [(2,4);(2,6)])

    [<Test>]
    let ``Knight's tour explores all squares on a 5x5 board exactly once, and each move is valid knight move``() =
        let sz = 5
        let b = Chess.Board.empty sz

        let validTour path = 
            let f (i,j) next_move = Set.contains next_move (Chess.Board.legalMoves b black_knight (i,j))   
            Seq.forall2 f path (List.tail path)

        match Chess.KnightsTour.tour (1,1) sz with
        | Some path -> 
            // visited all squares
            Assert.AreEqual(Set.ofList path, List.collect (fun i -> [for j in [1..sz] -> (i,j)]) [1..sz] |> Set.ofList)
            // each move was a valid move
            Assert.IsTrue(validTour path)
        | None      -> Assert.Fail()

    [<Test>]
    let ``There are 92 possible ways to place 8 queens on an 8x8 chessboard so that no two queens are attacking each other``() =
        Assert.AreEqual(Queens.queens 8 |> List.length, 92)