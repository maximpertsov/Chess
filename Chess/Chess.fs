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

    /// Return all permutations of a list
    let permutations' xs = permutations (List.length xs) xs

module Piece =
    type Color  = White | Black
    type Figure = King | Queen | Bishop | Knight | Rook | Pawn
    type Piece  = Color * Figure

    let piece c fig = (c,fig)

    let piece' fig c = (fig,c)

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
 
    let empty n : board = [|for _ in [1..n] -> [|for _ in [1..n] -> None|]|]

    let get i j (b : board) = Array.get b.[i-1] (j-1)
 
    let setPiece p i j (b : board) = Array.set b.[i-1] (j-1) (Some p); b
 
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

    /// returns a path moves created by moving a piece in series of one space moves in the direction given by (di, dj)
    /// move stops the pieces reaches another piece of the same color, the end of the board, or captures another piece
    let moveHelper p i j b (di, dj) =
        let rec loop i j ms =
            let i',j' = (i + di, j + dj)
            if isEnemy p i j b || isOffBoard i' j' b || isAlly p i' j' b 
            then ms
            else loop i' j' ((i',j')::ms)
        loop i j []

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

    // The Black Queen
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

    // The White Knight
    let WK = CP.piece CP.White CP.Knight

    let validDest (i,j) b =
        let n = Board.size b
        min i j > 0 && max i j <= n 

    let validMoves (i,j) b =
        let n = Board.size b

        let ms = Combinatorics.permutations 2 [1;-1;2;-2] |> List.filter (fun [i;j] -> abs i <> abs j)
        let f acc m = if validDest m b then m::acc else acc
        List.fold f [] (List.map (fun [i;j] -> i,j) ms)