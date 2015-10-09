namespace Chess

module Combinatorics =

    /// Return all possible sets of size k from a list
    let combos k xs =
        let rec subsets xs =
            match xs with
            | []     -> [[]]
            | x::xs' -> let subs = subsets xs'
                        subs @ [for s in subs -> x::s]
        [for s in subsets xs do if List.length s = k then yield s]

    /// Find all possible permutations of size k from a list 
    let permutations k xs =
        let rec loop ps =
            match ps with
            | []   -> [[]]
            | p::_ -> if List.length p = k then ps
                      else let f x = [for p in ps do if List.forall ((<>) x) p then yield x::p]        
                           loop (List.collect f xs)        
        loop [for x in xs -> [x]]

    /// Find all possible 2-element permutations in a list and return the result as list of 2-tuples
    let permutations2 xs = 
        let f p =
            match p with
            | [x1;x2] -> x1,x2
            | _       -> failwith "Encountered list with more or less than two elements"
        List.map f (permutations 2 xs)
        
    /// Return all permutations of a list
    let permutations' xs = permutations (List.length xs) xs