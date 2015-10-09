namespace Chess

module Board =
    type space = Piece.Piece option
    type board = space[][]
    type position = int * int
    type move = 
        | Move of Piece.Piece * position * position
        | Castle of (Piece.Piece * position * position) * (Piece.Piece * position * position)
        | EnPassant of (Piece.Piece * position * position) * (Piece.Piece * position * position)

    /// Add two coordinates together
    let (++) (i, j) (di, dj) = (i + di, j + dj)

    let size (b : board) = Array.length b
 
    /// Return an empty n x n board
    let empty n : board = [|for _ in [1..n] -> [|for _ in [1..n] -> None|]|]

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
        
    /// Return the space located on coordinates i,j of the chessboard
    let get (b : board) (i,j) = Array.get b.[i-1] (j-1)
 
    /// Return a new board with a new piece in i,j
    let setPiece (b : board) p pos = Array.mapi (fun i row -> Array.mapi (fun j spc -> if pos = (i+1,j+1) then Some p else spc) row) b

    /// Return a new board with many new pieces
    let setPieces (b : board) = Seq.fold (fun b' (p, i, j) -> setPiece b' p (i,j)) b

    /// Destructive version of setPiece
    let setPiece' (b : board) p (i, j) = Array.set b.[i-1] (j-1) (Some p); b

    /// Destructively set many pieces onto the board
    let setPieces' (b : board) = Seq.fold (fun b' (p, i, j) -> setPiece' b' p (i,j)) b
 
    /// Create a standard chess board
    let standard : board = 
        let setPawns c i (b : board) = Array.set b (i-1) [|for _ in b.[i-1] -> Some (c, Piece.Pawn)|]; b
 
        let setBackRow c i (b : board) =
            let rnc = [|Piece.Rook; Piece.Knight; Piece.Bishop|]
            let qk  = [|Piece.Queen; Piece.King|]
            Array.concat [rnc; qk; Array.rev rnc] |> Array.map (Some << Piece.piece c) |> Array.set b (i-1); b

        let white, black = Piece.White, Piece.Black

        empty 8 |> setBackRow white 1 |> setPawns white 2 |> setPawns black 7 |> setBackRow black 8

    let isOffBoard b (pos : position) =
        let i,j = pos
        let n = size b
        i < 1 || j < 1 || i > n || j > n

    let isEnemy b p (pos : position) =
        match get b pos with
        | Some p' -> not (Piece.sameColor p p')
        | _       -> false

    let isAlly b p (pos : position) =
        match get b pos with
        | Some p' -> Piece.sameColor p p'
        | _       -> false

    let isEmpty b (pos : position) = 
        Option.isNone (get b pos)    

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
            | Some pos' -> if n > 0 then loop (n-1, pos', Set.add pos' posns) else posns
            | None      -> posns
        loop (n, pos, Set.empty)

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
        match p with
        | Piece.White, Piece.Pawn -> 
            match moves with
            | (Move ((Piece.Black, Piece.Pawn), (i',j'), (i'',_)))::_ -> if   i = size b - 3 && i' = size b - 3 && i'' = size b - 1 && abs (j - j') = 1
                                                                         then Set.add (i'+1,j') (legalMoves b p pos)
                                                                         else legalMoves b p pos
            | _ -> legalMoves b p pos
        | Piece.Black, Piece.Pawn -> 
            match moves with
            | (Move ((Piece.White, Piece.Pawn), (i',j'), (i'',_)))::_ -> if   i = 4 && i' = 4 && i'' = 2 && abs (j - j') = 1
                                                                         then Set.add (i'-1,j') (legalMoves b p pos)
                                                                         else legalMoves b p pos
            | _ -> legalMoves b p pos
        | _ -> legalMoves b p pos      

    let private spaceToString (s:space) =
        match s with
        | None   -> System.Char.ConvertFromUtf32(65343)
        | Some p -> Piece.toString p
 
    let private rowToString i =
        // adjust spacing for two digit numbers
        let f = if i >= 10 then sprintf "%i %s|" else sprintf " %i %s|"
        (f i) << (String.concat "|") << Array.map spaceToString

    /// helper function for show; converts board to list of strings including a header
    let private toRowStrings b = 
        let alphabet = [|for i in 65313..(65313 + 25) -> System.Char.ConvertFromUtf32 i|]
        let header = sprintf "   %s " (alphabet.[0 .. Array.length b - 1] |> String.concat " ")
        let board  = Array.mapi (fun i -> rowToString (i+1)) b |> List.ofArray
        header::board

    let showInColumns ncols bs =
        let rec loop2 acc bn = 
            match bn with
            | []::_ -> acc
            | _     -> let s = List.map List.head bn |> String.concat " "
                       loop2 (s::acc) (List.map List.tail bn)
        let rec loop1 ans acc m bs =
            match acc, m, bs with
            | [], _, []          -> ans
            | _, 0, _ | _, _, [] -> let ans' = ans @ (loop2 [] (List.rev acc))
                                    loop1 ans' [] ncols bs
            | _, _, b::bs'       -> loop1 ans (b::acc) (m-1) bs'
        loop1 [] [] ncols (List.map toRowStrings bs) |> String.concat "\n"

    let show b = showInColumns 1 [b]

    let printInColumns ncols = (printf "%s\n") << showInColumns ncols

    let print b = printInColumns 1 [b]