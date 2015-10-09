namespace Chess

module Piece =
    type Color  = White | Black
    type Figure = King | Queen | Bishop | Knight | Rook | Pawn
    type Piece  = Color * Figure //TODO: make this a record
    
    let piece c fig : Piece = (c,fig)

    let piece' cStr figStr : Piece = 
        let c = match System.Char.ToUpper cStr with
                | 'W' -> White
                | 'B' -> Black
                | _   -> failwith "Invalid color: enter 'W' for white or 'B' for black"
        let fig = match System.Char.ToUpper figStr with
                  | 'K' -> King
                  | 'Q' -> Queen
                  | 'B' -> Bishop
                  | 'N' -> Knight
                  | 'R' -> Rook
                  | 'P' -> Pawn
                  | _   -> failwith "Invalid piece: enter 'K' (king), 'Q' (queen), 'B' (bishop), 'N' (knight), 'R' (rook), or 'P' (pawn)"
        (c, fig)

    let figure (p : Piece) = snd p

    let color (p: Piece) = fst p

    let sameColor p1 p2 = color p1 = color p2

    /// Return set of all shortest-distance moves, not factoring in position or chessboard configuration
    // TODO: consider adding a "possibleCapture" function that separately indicates what moves a piece can capture other pieces with
    //     : -- a "possibleCapture" function would make coding for pawn movement much less convoluted, and provide an approach to coding
    //     : -- other boardgame pieces with capturing movements that are different from their normal movements, 
    //     : -- such as the Chinese Chess cannon
    let rec possibleSteps (p : Piece) =
        match p with
        | _, Rook   -> [(1,0);(-1,0);(0,1);(0,-1)]
        | _, Bishop -> [(1,1);(-1,1);(1,-1);(-1,-1)]
        | c, Queen  -> possibleSteps (c, Rook) @ possibleSteps (c, Bishop)
        | c, King   -> possibleSteps (c, Queen)
        | _, Knight -> [for (i,j) in Combinatorics.permutations2 [1;-1;2;-2] do if abs i <> abs j then yield (i,j)]
        | White, Pawn -> [for j in [-1..1] -> (1,j)]
        | Black, Pawn -> [for (i,j) in possibleSteps (White, Pawn) -> -i,j]

    let toString p =
        // Unicode mapping for White Board pieces (Black pieces are the next six codes)
        let unicodeMap = Map.ofList (List.zip [King; Queen; Rook; Bishop; Knight; Pawn;] [9812 .. 9817])

        let rec unicodeNum p = 
            match p with
            | Black, fig -> unicodeNum (White, fig) + 6
            | _, fig     -> Map.find fig unicodeMap
            
        System.Char.ConvertFromUtf32 (unicodeNum p)