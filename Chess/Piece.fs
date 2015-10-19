namespace Chess

module Piece =
    type Color  = White | Black
    type Figure = King | Queen | Bishop | Knight | Rook | Pawn
    type Piece  = 
        private {color: Color; figure: Figure}
    with
        override this.ToString() = 
            // Unicode mapping for White Board pieces (Black pieces are the next six codes)
            let unicodeMap = 
                Map.ofList (List.zip [King; Queen; Rook; Bishop; Knight; Pawn] [9812 .. 9817])

            let rec unicodeNum c fig = 
                match c with
                | Black -> (unicodeNum White fig) + 6
                | White -> Map.find fig unicodeMap
            
            System.Char.ConvertFromUtf32 (unicodeNum this.color this.figure)

    let create (c:Color) (fig:Figure) : Piece = {color=c; figure=fig}

    let create' (color:string) (figure:string) : Piece =
        let c = 
            match String.map System.Char.ToLower color with
            | "w" | "white" -> White
            | "b" | "black" -> Black
            | _  -> failwith "Invalid color: enter 'W' for white or 'B' for black"
        let fig =  
            match String.map System.Char.ToLower figure with
            | "k" | "king"   -> King
            | "q" | "queen"  -> Queen
            | "b" | "bishop" -> Bishop
            | "n" | "knight" -> Knight
            | "r" | "rook"   -> Rook
            | "p" | "pawn"   -> Pawn
            | _  -> failwith "Invalid piece: enter 'K' (king), 'Q' (queen), 'B' (bishop), 'N' (knight), 'R' (rook), or 'P' (pawn)"
        {color=c; figure=fig}

    let create'' (color_figure:string) : Piece =
        match color_figure.Split [|' ';','|] with
        | [|arg1; arg2|] -> 
            try 
                create' arg1 arg2
            with
            | _ -> create' arg2 arg1
        | _ -> failwith "Invalid piece"
    
    let figure (p : Piece) = p.figure

    let color (p: Piece) = p.color

    let sameColor p1 p2 = color p1 = color p2

    /// Return set of all shortest-distance moves, not factoring in position or chessboard configuration
    // TODO: consider adding a "possibleCapture" function that separately indicates what moves a piece can capture other pieces with
    //     : -- a "possibleCapture" function would make coding for pawn movement much less convoluted, and provide an approach to coding
    //     : -- other boardgame pieces with capturing movements that are different from their normal movements, 
    //     : -- such as the Chinese Chess cannon
    let rec possibleSteps (p : Piece) =
        match (color p, figure p) with
        | _, Rook   -> [(1,0);(-1,0);(0,1);(0,-1)]
        | _, Bishop -> [(1,1);(-1,1);(1,-1);(-1,-1)]
        | c, Queen  -> possibleSteps (create c Rook) @ possibleSteps (create c Bishop)
        | c, King   -> possibleSteps (create c Queen)
        | _, Knight -> [for (i,j) in Combinatorics.permutations2 [1;-1;2;-2] do if abs i <> abs j then yield (i,j)]
        | White, Pawn -> [for j in [-1..1] -> (1,j)]
        | Black, Pawn -> [for (i,j) in possibleSteps (create White Pawn) -> -i,j]

//    let toString p =
//        // Unicode mapping for White Board pieces (Black pieces are the next six codes)
//        let unicodeMap = Map.ofList (List.zip [King; Queen; Rook; Bishop; Knight; Pawn;] [9812 .. 9817])
//
//        let rec unicodeNum p = 
//            match (color p, figure p) with
//            | Black, fig -> unicodeNum (create White fig) + 6
//            | _, fig     -> Map.find fig unicodeMap
//            
//        System.Char.ConvertFromUtf32 (unicodeNum p)