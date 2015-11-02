namespace Chess

module Board =
    type Space = 
        private
        | Empty
        | Piece of Piece.Piece
    with
        override this.ToString() =
            match this with
            | Empty   -> System.Char.ConvertFromUtf32(65343)
            | Piece p -> p.ToString()

    type position = int * int

    type Board =
//        private {size: int; spaces: Space[][]}
        private {size: int; spaces: Map<position,Space>}

    type move = 
        | Move of Piece.Piece * position * position
        | Castle of (Piece.Piece * position * position) * (Piece.Piece * position * position)
        | EnPassant of (Piece.Piece * position * position) * (Piece.Piece * position * position)

    /// Add two coordinates together
    let (++) (i, j) (di, dj) = (i + di, j + dj)
 
    /// Return an empty n x n board
    let empty n : Board = 
//        {size=n; spaces=[|for _ in [1..n] -> [|for _ in [1..n] -> Empty|]|]}
        {size=n; spaces=Map.empty}

    // Get the size of one side of a board
    let size (b : Board) = b.size

    /// Check if a position is off the board
    let isOffBoard b (i,j) =
        let n = size b
        i < 1 || j < 1 || i > n || j > n

    /// Return the space located on coordinates i,j of the chessboard
    let get (b : Board) (i,j) = 
        if isOffBoard b (i,j) then 
            failwith "Space (%i,%i) is not on the board" i j
        else
//            Array.get b.spaces.[i-1] (j-1)
            match Map.tryFind (i,j) b.spaces with
            | None   -> Empty
            | Some p -> p

    /// Return a new board with a new piece in i,j
    let setPiece (b : Board) p pos = 
        if isOffBoard b pos then 
            failwith "Space (%i,%i) is not on the board" (fst pos) (snd pos)
        else
//            {size=(size b); spaces=Array.mapi (fun i row -> Array.mapi (fun j spc -> if pos = (i,j) ++ (1,1) then Piece p else spc) row) b.spaces}
            {size=(size b); spaces=Map.add pos (Piece p) b.spaces}

    /// Return a new board with many new pieces
    let setPieces (b : Board) = Seq.fold (fun b' (p, i, j) -> setPiece b' p (i,j)) b
 
    /// Create a standard chess board
    let standard : Board = 
        let setPawns c i (b : Board) = setPieces b [for j in 1..(size b) -> (Piece.create' c "pawn", i, j)]
     
        let setBackRow c i (b : Board) = 
            let backPieces c = List.map (Piece.create' c) ["rook"; "knight"; "bishop"; "queen"; "king"; "bishop"; "knight"; "rook"]
            setPieces b (List.zip3 (backPieces c) [for _ in 1..(size b) -> i] [for j in 1..(size b) -> j])

        empty 8 |> setBackRow "white" 1 |> setPawns "white" 2 |> setPawns "black" 7 |> setBackRow "black" 8

    let isEnemy b p (pos : position) =
        match get b pos with
        | Piece p' -> not (Piece.sameColor p p')
        | _       -> false

    let isAlly b p (pos : position) =
        match get b pos with
        | Piece p' -> Piece.sameColor p p'
        | _       -> false

    let isEmpty b (pos : position) = 
        match get b pos with
        | Empty -> true
        | _     -> false   

    /// returns a set of positions created by moving a piece n times in series of one space moves in the direction given by (di, dj)
    /// move stops the pieces reaches another piece of the same color, the end of the board, or captures another piece
    let moveN b n p pos delta =
        let move1 pos =
            let pos' = pos ++ delta
            if isEnemy b p pos || isOffBoard b pos' || isAlly b p pos'
            then None
            else Some pos'
        let rec loop (n, pos, posns) =
            match move1 pos with
            | Some pos' -> if n > 0 then loop (n-1, pos', Set.add pos' posns) 
                           else posns
            | None      -> posns
        loop (n, pos, Set.empty)

    /// returns a set of positions created by moving a piece n times in series of one space moves in the direction given by (di, dj)
    /// move stops the pieces reaches another piece of the same color, the end of the board, or captures another piece
    let moveN' b n pos delta =
        match get b pos with
        | Empty   -> failwith "No Pieces on space (%i,%i)" (fst pos) (snd pos)
        | Piece p -> moveN b n p pos delta

    /// returns the set of legal moves of a piece at a given position on the board
    let rec legalMoves b (p : Piece.Piece) pos =
        let i,j = pos
        let move n p = List.map (moveN b n p pos) (Piece.possibleSteps p) |> Set.unionMany
        match Piece.figure p with
        | Piece.Rook | Piece.Bishop | Piece.Queen -> move (size b) p        // TODO: add castling for Rook
        | Piece.King | Piece.Knight               -> move 1 p               // TODO: add castling for King
        | Piece.Pawn ->                                                     // TODO: add en passant for Pawn
            let isInitPos =
                match Piece.color p with
                | Piece.White -> i = 2
                | Piece.Black -> i = size b - 1
            move 2 p 
            // only allow diagonal moves if capturing another piece
            |> Set.filter (fun (i',j') -> (j = j' && isEmpty b (i', j')) || (j <> j' && isEnemy b p (i', j'))) 
            // only allow two-space moves when in initial position
            |> Set.filter (fun (i',j') -> (abs (i' - i) < 2 || isInitPos) && abs (j' - j) < 2)

    /// returns the set of legal moves of a piece at a given position on the board, and also considering all prior moves
    //  TODO: test this function!!!
    let rec legalMoves' b moves (p : Piece.Piece) pos =
        let i,j = pos
        match (Piece.color p, Piece.figure p) with
        | Piece.White, Piece.Pawn -> 
            match moves with
            | (Move (p', (i',j'), (i'',_)))::_ -> 
                if not (Piece.sameColor p p') && i = size b - 3 && i' = size b - 3 && i'' = size b - 1 && abs (j - j') = 1
                    then Set.add (i'+1,j') (legalMoves b p pos)
                    else legalMoves b p pos
            | _ -> legalMoves b p pos
        | Piece.Black, Piece.Pawn -> 
            match moves with
            | (Move (p', (i',j'), (i'',_)))::_ -> 
                if not (Piece.sameColor p p') && i = 4 && i' = 4 && i'' = 2 && abs (j - j') = 1
                    then Set.add (i'-1,j') (legalMoves b p pos)
                    else legalMoves b p pos
            | _ -> legalMoves b p pos
        | _ -> legalMoves b p pos      

    /// helper function for show; converts board to list of strings including a header
    let private toRowStrings (b:Board) = 
        let alphabet = [|for i in 65313..(65313 + 25) -> System.Char.ConvertFromUtf32 i|]
        let header = alphabet.[0 .. (size b - 1)] |> String.concat " " |> sprintf "   %s "
        let board  = 
            let rowToString i =
                [|for j in 1..(size b) -> get b (i,j)|]
                |> Array.map (sprintf "%A")
                |> (String.concat "|")
                // adjust spacing for two digit numbers
                |> (if i >= 10 then sprintf "%i %s|" else sprintf " %i %s|") i
            [for i in 1..(size b) -> rowToString i]
        header::board

    let showInColumns column_count bs =
        let rec loop2 acc bn = 
            match bn with
            | []::_ -> acc
            | _     -> let s = List.map List.head bn |> String.concat " "
                       loop2 (s::acc) (List.map List.tail bn)
        let rec loop1 ans acc m bs =
            match acc, m, bs with
            | [], _, []          -> ans
            | _, 0, _ | _, _, [] -> let ans' = ans @ (loop2 [] (List.rev acc))
                                    loop1 ans' [] column_count bs
            | _, _, b::bs'       -> loop1 ans (b::acc) (m-1) bs'
        loop1 [] [] column_count (List.map toRowStrings bs) |> String.concat "\n"

    let show b = showInColumns 1 [b]

    let printInColumns column_count = (printf "%s\n") << (showInColumns column_count)

    let print b = printInColumns 1 [b]

    //let regex = System.Text.RegularExpressions.Regex.Match("Kn3","[A-Z]?[a-n][0-9]{1,2}")

    /// Convert a rank and file to a set of coordinates (for now does not allow file labels beyond the letter Z)
    //    let positionFromAlgebraic (s : string) =
    //        let s' = s.ToLower()
    //        try 
    //            let file = s'.[0]
    //            let rank = System.Double.TryParse s'.[1..]
    //        with
    //        | _ -> failwith "Invalid algebraic notation"

        // Check if valid coordinates