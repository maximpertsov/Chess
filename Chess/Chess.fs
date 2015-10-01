namespace Chess

module Combinatorics =

    /// Return all possible sets of size k from a list
    let combos k xs =
        let rec subsets xs =
            match xs with
            | []     -> [[]]
            | x::xs' -> let subs' = subsets xs'
                        subs' @ (List.map (fun ys -> x::ys) subs')
        List.filter (fun ys -> List.length ys = k) (subsets xs) |> List.rev

    /// Find all possible permutations of size k from a list 
    let permutations k xs =
        let rec loop ps =
            match ps with
            | []     -> [[]]
            | xs'::_ -> if List.length xs' = k then ps
                        else let g x ys  = if (List.exists ((=) x) ys) then [] else x::ys
                             let ps' = List.collect (fun x -> List.map (g x) ps) xs                 
                             loop (List.filter (not << List.isEmpty) ps')         
        loop (List.map (fun x -> [x]) xs)

    /// Find all possible 2-element permutations in a list and return the result as list of 2-tuples
    let permutations2 xs = 
        let f p =
            match p with
            | [x1;x2] -> x1,x2
            | _       -> failwith "Encountered list with more or less than two elements"
        List.map f (permutations 2 xs)

    /// Return all permutations of a list
    let permutations' xs = permutations (List.length xs) xs

module Piece =
    type Color  = White | Black
    type Figure = King | Queen | Bishop | Knight | Rook | Pawn
    type Piece  = Color * Figure

    let piece c fig = (c,fig)

    let piece' fig c = (fig,c)

    /// Return set of all shortest-distance moves, not factoring in position or chess board configuration
    let rec possibleMoves1 (p : Piece) =
        match p with
        | _, Rook   -> [(1,0);(-1,0);(0,1);(0,-1)]
        | _, Bishop -> [(1,1);(-1,1);(1,-1);(-1,-1)]
        | c, Queen  -> List.concat [possibleMoves1 (c, Rook); possibleMoves1 (c, Bishop)]
        | c, King   -> possibleMoves1 (c, Queen)
        | _, Knight -> Combinatorics.permutations2 [1;-1;2;-2] |> List.filter (fun (x1,x2) -> abs x1 <> abs x2)
        | _ -> failwith "Not implemented"

    let toString p =
        // Unicode mapping for White Board pieces (Black pieces are the next six codes)
        let unicodeMap = Map.ofList (List.zip [King; Queen; Rook; Bishop; Knight; Pawn;] [9812 .. 9817])

        let rec unicodeNum p = 
            match p with
            | Black, fig -> unicodeNum (White, fig) + 6
            | _, fig     -> Map.find fig unicodeMap

        System.Char.ConvertFromUtf32 (unicodeNum p)

module Board =
    type space = Piece.Piece option
    type board = space[][]

    let size (b : board) = Array.length b
 
    /// Return an empty n x n board
    let empty n : board = [|for _ in [1..n] -> [|for _ in [1..n] -> None|]|]

    let get i j (b : board) = Array.get b.[i-1] (j-1)
 
    /// Return a new board with a new piece in i,j
    let setPiece p i j (b : board) = Array.mapi (fun i' r -> Array.mapi (fun j' p' ->  if i-1 = i' && j-1 = j' then Some p else p') r) b

    /// Destructive version of setPiece
    let setPiece' p i j (b : board) = Array.set b.[i-1] (j-1) (Some p); b
 
    /// Create a standard chess board
    let standard : board = 
        let setPawns c i (b : board) = Array.set b (i-1) [|for _ in b.[i-1] -> Some (c, Piece.Pawn)|]; b
 
        let setBackRow c i (b : board) =
            let rnc = [|Piece.Rook; Piece.Knight; Piece.Bishop|]
            let qk  = [|Piece.Queen; Piece.King|]
            Array.concat [rnc; qk; Array.rev rnc] |> Array.map (Some << Piece.piece c) |> Array.set b (i-1); b
        let white, black = Piece.White, Piece.Black
        empty 8 |> setBackRow white 1 |> setPawns white 2 |> setPawns black 7 |> setBackRow black 8

    let isOffBoard i j b =
        let n = size b
        i < 1 || j < 1 || i > n || j > n

    let isEnemy p i j b =
        match p, get i j b with
        | (c1, _), Some (c2, _) -> c1 <> c2
        | _                     -> false

    let isAlly p i j b =
        match p, get i j b with
        | (c1, _), Some (c2, _) -> c1 = c2
        | _                     -> false

    /// Check if piece can move once in the direction given by (di, dj). Returns the next position or None.
    let moveHelper1 p i j b (di, dj) =
        let i',j' = (i + di, j + dj)
        if isEnemy p i j b || isOffBoard i' j' b || isAlly p i' j' b 
        then None
        else Some (i',j')        

    /// returns a set of moves created by moving a piece in series of one space moves in the direction given by (di, dj)
    /// move stops the pieces reaches another piece of the same color, the end of the board, or captures another piece
    let moveHelper p i j b (di, dj) =
        let rec loop (i,j) ms =
            match moveHelper1 p i j b (di,dj) with
            | Some m -> loop m (Set.add m ms)
            | None   -> ms
        loop (i,j) Set.empty


(*
    let rec legalMoves' p i j b =
        let orthogonals = [(1,0);(-1,0);(0,1);(0,-1)]
        let diagonals   = [(1,1);(-1,1);(1,-1);(-1,-1)]
        let eightways   = (orthogonals @ diagonals)
        let knightmoves = Combinatorics.permutations2 [1;-1;2;-2] |> List.filter (fun (x1,x2) -> abs x1 <> abs x2)
        let move1 p = Set.ofList << (List.collect ((Option.toList) << (moveHelper1 p i j b)))
        let moveN p = Set.unionMany << (List.map (moveHelper p i j b))
        match p with
        | _, Piece.Rook   -> moveN p orthogonals
        | _, Piece.Bishop -> moveN p diagonals
        | _, Piece.Queen  -> moveN p eightways
        | _, Piece.King   -> move1 p eightways
        | _, Piece.Knight -> move1 p knightmoves
        | _ -> failwith "Not implemented"
*)

    /// returns the set of legal moves of a piece at a given position on the board
    // TODO: Add ways to prevent moves that put your own king in check, allow for castling, etc
    //       It might be better to add them in a separate function that checks for contextual moves
    let rec legalMoves (p : Piece.Piece) i j b =
        let move1 p = List.collect ((Option.toList) << (moveHelper1 p i j b)) (Piece.possibleMoves1 p) |> Set.ofList
        let moveN p = List.map (moveHelper p i j b) (Piece.possibleMoves1 p) |> Set.unionMany
        match p with
        | _, Piece.Rook | _, Piece.Bishop | _, Piece.Queen -> moveN p
        | _, Piece.King | _, Piece.Knight                  -> move1 p
        | _ -> failwith "Not implemented"
        
    let private spaceToString s =
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

module Queens =
    module CP = Piece

    /// The Black Queen
    let BQ = CP.piece CP.Black CP.Queen
     
    let noDiagonalAttacks xs =
        let f i st x = Set.add (x + i) (Set.map ((+) i) st)
        let unsafeDiags = List.map2 Set.union (List.scan (f 1) Set.empty xs) (List.scan (f -1) Set.empty xs) |> Array.ofList
        List.mapi (fun i x -> Set.contains x unsafeDiags.[i]) xs |> List.forall (not << id)
     
    let queens n = List.filter noDiagonalAttacks (Combinatorics.permutations' [1..n])

    let private queensToBoard p =
        let n = List.length p
        let coords = List.mapi (fun j x -> (x,j+1)) p
        List.fold (fun b (i,j) -> Board.setPiece BQ i j b) (Board.empty n) coords

    let show ncols = (Board.showInColumns ncols) << (List.map queensToBoard) << queens

    let print ncols = (Board.printInColumns ncols) << (List.map queensToBoard) << queens

module KnightsTour =
    module CP = Piece

    /// The Black Knight
    let BK = CP.piece CP.Black CP.Knight

    /// Find a path (if any) starting from position i j, that explores an entire n x n chessboard 
    let tour i j n =
        let p = BK
        let b = Board.empty n
        let rec loop (path, visited, size) =
            // Have we explored every square? If so, our tour is complete!
            if   size = n * n
            then Some (List.rev path)
            else let i,j = List.head path
                 // Otherwise, what new squares can we explore from our current square?
                 let mset = (Board.legalMoves p i j b) - visited
                 //printf "squares visited: %i\nnext available squares: %O\n" size mset
                 if Set.isEmpty mset then None
                 // Do any of those new squares eventually lead to a complete tour of the chessboard?
                 else let f ans m = match ans with 
                                    | None -> loop (m::path, Set.add m visited, size + 1) 
                                    | _    -> ans
                      Set.fold f None mset
        loop ([(i,j)], Set.singleton (i,j), 1)

    /// Convert a knight's step into a board string
    let tourToBoard n (i,j) =
        Board.setPiece' BK i j (Board.empty n)

    /// Show the knight's tour
    let show ncols i j n =
        match tour i j n with
        | None   -> sprintf "Knight cannot explore an entire %ix%i board from position %i,%i\n" n n i j
        | Some t -> List.map (tourToBoard n) t |> Board.showInColumns ncols

    /// Print the knight's tour
    let print ncols i j n =
        match tour i j n with
        | None   -> printf "Knight cannot explore an entire %ix%i board from position %i,%i\n" n n i j
        | Some t -> List.map (tourToBoard n) t |> Board.printInColumns ncols