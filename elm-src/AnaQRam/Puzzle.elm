module AnaQRam.Puzzle exposing (Piece, Puzzle, display, dummy, empty, getPiece, init, map, pieceToString, problem, problems, shuffle, size, start, success, swapPiece)

import Array exposing (Array)
import Dict exposing (Dict)
import Random
import Random.Array as Array
import Random.List as List


type alias Puzzle =
    { pieces : Array Piece
    , answer : String
    , start : Bool
    }


type alias Piece =
    { hidden : Bool
    , index : Int
    , char : Char
    }


init : String -> Puzzle -> Puzzle
init answer puzzle =
    let
        pieces =
            String.toList answer
                |> Array.fromList
                |> Array.indexedMap (Piece True)
    in
    { puzzle | pieces = pieces, answer = answer }


start : Puzzle -> Puzzle
start puzzle =
    { puzzle | start = True }


shuffle : (Puzzle -> msg) -> Puzzle -> Cmd msg
shuffle toMsg puzzle =
    Random.generate
        (\updated -> toMsg { puzzle | pieces = updated })
        (Array.shuffle puzzle.pieces)


empty : Puzzle
empty =
    Puzzle Array.empty "" False


size : Puzzle -> Int
size =
    Array.length << .pieces


display : Int -> Puzzle -> Puzzle
display idx puzzle =
    let
        pIdx =
            modBy (size puzzle) idx

        updated =
            Array.map (displayPiece pIdx) puzzle.pieces
    in
    { puzzle | pieces = updated }


displayPiece : Int -> Piece -> Piece
displayPiece idx piece =
    if piece.index == idx then
        { piece | hidden = False }

    else
        piece


success : Puzzle -> Bool
success puzzle =
    case puzzle.answer of
        "" ->
            False

        _ ->
            Array.map pieceToString puzzle.pieces
                |> Array.toList
                |> String.concat
                |> (==) puzzle.answer


map : (Int -> Piece -> a) -> Puzzle -> List a
map f puzzle =
    puzzle.pieces
        |> Array.indexedMap f
        |> Array.toList


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
            modBy (size puzzle) idx
    in
    Array.filter (\p -> p.index == pIdx) puzzle.pieces
        |> Array.get 0


swapPiece : Int -> Int -> Puzzle -> Puzzle
swapPiece idxA idxB puzzle =
    case ( Array.get idxA puzzle.pieces, Array.get idxB puzzle.pieces ) of
        ( Just pieceA, Just pieceB ) ->
            let
                updated =
                    puzzle.pieces
                        |> Array.set idxB pieceA
                        |> Array.set idxA pieceB
            in
            { puzzle | pieces = updated }

        _ ->
            puzzle


problems : Dict Int (List String)
problems =
    let
        append v acc =
            Maybe.map ((::) v) acc
                |> Maybe.withDefault [ v ]
                |> Just

        update word =
            Dict.update (String.length word) (append word)
    in
    [ "りんご"
    , "ゴリラ"
    , "ラッパ"
    , "パンダ"
    , "スイス"
    , "スライド"
    , "トンネル"
    , "アップル"
    , "オレンジ"
    , "パソコン"
    , "ハリネズミ"
    , "とうきょう"
    , "エベレスト"
    , "ランドセル"
    , "カブトムシ"
    , "カルボナーラ"
    , "オリンピック"
    , "スマートホン"
    , "しんかんせん"
    , "やまのてせん"
    , "あなくらむ！"
    ]
        |> List.foldl update Dict.empty


problem : (String -> msg) -> Int -> Cmd msg
problem toMsg wordSize =
    Dict.get wordSize problems
        |> Maybe.withDefault []
        |> List.shuffle
        |> Random.generate (toMsg << Maybe.withDefault "" << List.head)


dummy : Int -> Puzzle
dummy wordSize =
    init (String.padRight wordSize ' ' "") empty
