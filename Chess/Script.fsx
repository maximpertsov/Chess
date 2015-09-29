// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Chess.fs"
open Chess
open System.IO

File.WriteAllText ("/Users/mpertsov/Projects/Chess/Chess/queens.txt", Queens.show 4 8);;
File.WriteAllText ("/Users/mpertsov/Projects/Chess/Chess/standard.txt", Board.show Board.standard);;