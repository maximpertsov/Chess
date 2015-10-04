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
        | c, Queen  -> List.concat [possibleSteps (c, Rook); possibleSteps (c, Bishop)]
        | c, King   -> possibleSteps (c, Queen)
        | _, Knight -> Combinatorics.permutations2 [1;-1;2;-2] |> List.filter (fun (x1,x2) -> abs x1 <> abs x2)
        | White, Pawn -> [for j in [-1..1] -> (1,j)]
        | Black, Pawn -> possibleSteps (White, Pawn) |> List.map (fun (i,j) -> -1*i,j)

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
    type position = int * int

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
    let get (b : board) (pos : position) = 
        let i,j = pos
        Array.get b.[i-1] (j-1)
 
    /// Return a new board with a new piece in i,j
    let setPiece (b : board) p (i, j) = Array.mapi (fun i' r -> Array.mapi (fun j' p' ->  if i-1 = i' && j-1 = j' then Some p else p') r) b

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

    let isEmpty b (pos : position) = Option.isNone (get b pos)    

    /// returns a set of positions created by moving a piece n times in series of one space moves in the direction given by (di, dj)
    /// move stops the pieces reaches another piece of the same color, the end of the board, or captures another piece
    let moveN b n p pos delta =
        let (++) (i, j) (di, dj) = (i + di, j + dj)
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
            |> Set.filter (fun (i',j') -> (j = j' && isEmpty b (i', j')) || (j <> j' && isEnemy b p (i', j'))) 
            // only allow two-space moves when in initial position
            |> Set.filter (fun (i',j') -> (abs (i' - i) < 2 || isInitPos) && abs (j' - j) < 2)

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

module Queens =
    module CP = Piece

    /// The Black Queen
    let BQ = CP.piece CP.Black CP.Queen
     
    /// Return all possible ways to arrange n queens on an n x n chessboard, so that no two queens are attacking each other
    let queens n = 
        let noDiagonalAttacks xs =
            let f i st x = Set.add (x + i) (Set.map ((+) i) st)
            let unsafeDiags = List.map2 Set.union (List.scan (f 1) Set.empty xs) (List.scan (f -1) Set.empty xs) |> Array.ofList
            List.mapi (fun i x -> Set.contains x unsafeDiags.[i]) xs |> List.forall (not << id)
        List.filter noDiagonalAttacks (Combinatorics.permutations' [1..n])

    /// Helper method that transforms solutions into Boards, and then applies the specified output function to them
    let private outputHelper f ncols =
        let queensToBoard p =
            let n = List.length p
            let coords = List.mapi (fun j x -> (x,j+1)) p
            List.fold (fun b (i,j) -> Board.setPiece b BQ (i,j)) (Board.empty n) coords
        (f ncols) << (List.map queensToBoard) << queens

    let show ncols = outputHelper Board.showInColumns ncols //(Board.showInColumns ncols) << (List.map queensToBoard) << queens

    let print ncols = outputHelper Board.printInColumns ncols //(Board.printInColumns ncols) << (List.map queensToBoard) << queens

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