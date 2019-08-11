module AnaQRam.Puzzle exposing (Piece, Puzzle, display, empty, getPiece, init, map, pieceToString, shuffle, success, swapPiece)

import Array exposing (Array)
import Random
import Random.Array as Array


type alias Puzzle =
    Array Piece


type alias Piece =
    { hidden : Bool
    , index : Int
    , char : Char
    }


init : String -> Puzzle
init answer =
    String.toList answer
        |> Array.fromList
        |> Array.indexedMap (Piece True)


shuffle : (Puzzle -> msg) -> Puzzle -> Cmd msg
shuffle toMsg puzzle =
    Random.generate toMsg (Array.shuffle puzzle)


empty : Puzzle
empty =
    Array.empty


display : Int -> Puzzle -> Puzzle
display idx puzzle =
    let
        pIdx =
            modBy (Array.length puzzle) idx
    in
    Array.map (displayPiece pIdx) puzzle


displayPiece : Int -> Piece -> Piece
displayPiece idx piece =
    if piece.index == idx then
        { piece | hidden = False }

    else
        piece


success : String -> Puzzle -> Bool
success answer puzzle =
    Array.map .char puzzle
        |> Array.toList
        |> String.fromList
        |> (==) answer


map : (Int -> Piece -> a) -> Puzzle -> List a
map f puzzle =
    Array.toList (Array.indexedMap f puzzle)


pieceToString : Piece -> String
pieceToString piece =
    if piece.hidden then
        "？"

    else
        String.fromChar piece.char


getPiece : Int -> Puzzle -> Maybe Piece
getPiece idx puzzle =
    let
        pIdx =
            modBy (Array.length puzzle) idx
    in
    Array.filter (\p -> p.index == pIdx) puzzle
        |> Array.get 0


swapPiece : Int -> Int -> Puzzle -> Puzzle
swapPiece idxA idxB puzzle =
    case ( Array.get idxA puzzle, Array.get idxB puzzle ) of
        ( Just pieceA, Just pieceB ) ->
            puzzle
                |> Array.set idxB pieceA
                |> Array.set idxA pieceB

        _ ->
            puzzle
