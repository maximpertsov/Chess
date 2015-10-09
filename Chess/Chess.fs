namespace Chess

module Queens =
    module CP = Piece

    /// The Black Queen
    let BQ = CP.piece CP.Black CP.Queen
     
    /// Return all possible ways to arrange n queens on an n x n chessboard, so that no two queens are attacking each other
    let queens n = 
        let noDiagonalAttacks rows =
            // at each column, determine which rows are being attacked diagonally by all queens to the left
            let unsafeRows rows = 
                let f col set' row = Set.add (row + col) (Set.map ((+) col) set')
                List.map2 Set.union (List.scan (f 1) Set.empty rows) (List.scan (f -1) Set.empty rows)
            // remove all configurations where a queen is diagonally attacking a queen to the right of it
            Seq.forall2 (fun row rows' -> not (Set.contains row rows')) rows (unsafeRows rows)

        List.filter noDiagonalAttacks (Combinatorics.permutations' [1..n])

    /// Helper method that transforms solutions into Boards, and then applies the specified output function to them
    let private outputHelper f column_count =
        let queensToBoard p =
            let posns = List.mapi (fun col row -> (row, col+1)) p
            List.fold (fun board pos -> Board.setPiece board BQ pos) (Board.empty (List.length p)) posns
        (f column_count) << (List.map queensToBoard) << queens

    let show column_count = outputHelper Board.showInColumns column_count

    let print column_count = outputHelper Board.printInColumns column_count

module KnightsTour =
    module CP = Piece

    /// The Black Knight
    let BK = CP.piece CP.Black CP.Knight

    /// Find a path (if any) starting from a given position that explores an entire n x n chessboard 
    let tour pos n =
        let p = BK
        let b = Board.empty n
        let rec loop (path, visited, size) =
            // Have we explored every square? If so, our tour is complete!
            if   size = n * n
            then Some (List.rev path)
            // What new squares can we explore from our current square? 
            else let mset = (Board.legalMoves b p (List.head path)) - visited            
                 if Set.isEmpty mset 
                 then None
                 // Do any of those new, unexplored squares eventually lead to a complete tour of the chessboard?
                 else let f ans pos' =        
                          match ans with 
                          | None -> loop (pos'::path, Set.add pos' visited, size + 1) // Not yet, try another square...
                          | _    -> ans                                               // Found one!
                      Set.fold f None mset
        loop ([pos], Set.singleton pos, 1)

    /// Convert a knight's step into a board string
    let tourToBoard n pos =
        Board.setPiece' (Board.empty n) BK pos 

    /// Show the knight's tour
    let show ncols pos n =
        let i,j = pos
        match tour pos n with
        | None   -> sprintf "Knight cannot explore an entire %ix%i board from position %i,%i\n" n n i j
        | Some t -> List.map (tourToBoard n) t |> Board.showInColumns ncols

    /// Print the knight's tour
    let print ncols pos n =
        let i,j = pos
        match tour pos n with
        | None   -> printf "Knight cannot explore an entire %ix%i board from position %i,%i\n" n n i j
        | Some t -> List.map (tourToBoard n) t |> Board.printInColumns ncols