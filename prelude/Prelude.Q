#' The Quone standard prelude.
#'
#' This file is implicitly imported into every user module. It defines the
#' initial-release primitive types, core custom types, fixed operator overloads,
#' common vectorized functions, reducers, and small Result helpers.

extern type Integer
extern type Double
extern type Character
extern type Vector a

type Logical <- True | False
type Maybe a <- Nothing | Just a
type Result e a <- Err e | Ok a

extern elementwise to_double : Integer -> Double = "as.double"
extern elementwise to_character : a -> Character = "as.character"

extern elementwise sqrt : Double -> Double = "sqrt"
extern elementwise abs : Double -> Double = "abs"
extern elementwise log : Double -> Double = "log"
extern elementwise exp : Double -> Double = "exp"
extern elementwise round : Double -> Double = "round"
extern elementwise floor : Double -> Double = "floor"
extern elementwise ceiling : Double -> Double = "ceiling"

extern elementwise map : (a -> b) -> Vector a -> Vector b = "purrr::map" { dispatch_on = "b" }
extern elementwise map2 : (a -> b -> c) -> Vector a -> Vector b -> Vector c = "purrr::map2"

extern reducer length : Vector a -> Integer = "length"
extern reducer count : Vector a -> Integer = "length"
extern reducer mean : Vector Double -> Double = "mean"
extern reducer median : Vector Double -> Double = "stats::median"
extern reducer sum : Vector Double -> Double = "sum"
extern reducer sd : Vector Double -> Double = "stats::sd"
extern reducer var : Vector Double -> Double = "stats::var"
extern reducer min : Vector Double -> Double = "min"
extern reducer max : Vector Double -> Double = "max"

extern elementwise if_else : Logical -> a -> a -> a = "dplyr::if_else"

extern elementwise str_detect : Character -> Character -> Logical = "stringr::str_detect"
extern elementwise str_to_upper : Character -> Character = "stringr::str_to_upper"
extern elementwise str_to_lower : Character -> Character = "stringr::str_to_lower"

with_default : a -> Maybe a -> a
with_default fallback m <-
    case m of
        Just x -> x
        Nothing -> fallback

result_map : (v -> w) -> Result e v -> Result e w
result_map f r <-
    case r of
        Ok v -> Ok (f v)
        Err e -> Err e

extern reducer panic : Character -> a = "stop"

expect : Result e a -> a
expect r <-
    case r of
        Ok v -> v
        Err _ -> panic "Result.expect failed"

infix left 6 (+) : Integer -> Integer -> Integer = "+"
infix left 6 (+) : Double -> Double -> Double = "+"
infix left 6 (+) : Vector Integer -> Vector Integer -> Vector Integer = "+"
infix left 6 (+) : Vector Integer -> Integer -> Vector Integer = "+"
infix left 6 (+) : Integer -> Vector Integer -> Vector Integer = "+"
infix left 6 (+) : Vector Double -> Vector Double -> Vector Double = "+"
infix left 6 (+) : Vector Double -> Double -> Vector Double = "+"
infix left 6 (+) : Double -> Vector Double -> Vector Double = "+"

infix left 6 (-) : Integer -> Integer -> Integer = "-"
infix left 6 (-) : Double -> Double -> Double = "-"
infix left 6 (-) : Vector Integer -> Vector Integer -> Vector Integer = "-"
infix left 6 (-) : Vector Integer -> Integer -> Vector Integer = "-"
infix left 6 (-) : Integer -> Vector Integer -> Vector Integer = "-"
infix left 6 (-) : Vector Double -> Vector Double -> Vector Double = "-"
infix left 6 (-) : Vector Double -> Double -> Vector Double = "-"
infix left 6 (-) : Double -> Vector Double -> Vector Double = "-"

infix left 7 (*) : Integer -> Integer -> Integer = "*"
infix left 7 (*) : Double -> Double -> Double = "*"
infix left 7 (*) : Vector Integer -> Vector Integer -> Vector Integer = "*"
infix left 7 (*) : Vector Integer -> Integer -> Vector Integer = "*"
infix left 7 (*) : Integer -> Vector Integer -> Vector Integer = "*"
infix left 7 (*) : Vector Double -> Vector Double -> Vector Double = "*"
infix left 7 (*) : Vector Double -> Double -> Vector Double = "*"
infix left 7 (*) : Double -> Vector Double -> Vector Double = "*"

infix left 7 (/) : Double -> Double -> Double = "/"
infix left 7 (/) : Vector Double -> Vector Double -> Vector Double = "/"
infix left 7 (/) : Vector Double -> Double -> Vector Double = "/"
infix left 7 (/) : Double -> Vector Double -> Vector Double = "/"

infix left 7 (//) : Integer -> Integer -> Integer = "%/%"
infix left 7 (//) : Vector Integer -> Vector Integer -> Vector Integer = "%/%"
infix left 7 (//) : Vector Integer -> Integer -> Vector Integer = "%/%"
infix left 7 (//) : Integer -> Vector Integer -> Vector Integer = "%/%"

infix left 7 (%) : Integer -> Integer -> Integer = "%%"
infix left 7 (%) : Vector Integer -> Vector Integer -> Vector Integer = "%%"
infix left 7 (%) : Vector Integer -> Integer -> Vector Integer = "%%"
infix left 7 (%) : Integer -> Vector Integer -> Vector Integer = "%%"

infix right 8 (^) : Double -> Double -> Double = "^"
infix right 8 (^) : Vector Double -> Vector Double -> Vector Double = "^"
infix right 8 (^) : Vector Double -> Double -> Vector Double = "^"
infix right 8 (^) : Double -> Vector Double -> Vector Double = "^"

infix non 4 (==) : Integer -> Integer -> Logical = "=="
infix non 4 (==) : Double -> Double -> Logical = "=="
infix non 4 (==) : Character -> Character -> Logical = "=="
infix non 4 (==) : Logical -> Logical -> Logical = "=="
infix non 4 (==) : Vector Integer -> Vector Integer -> Vector Logical = "=="
infix non 4 (==) : Vector Integer -> Integer -> Vector Logical = "=="
infix non 4 (==) : Integer -> Vector Integer -> Vector Logical = "=="
infix non 4 (==) : Vector Double -> Vector Double -> Vector Logical = "=="
infix non 4 (==) : Vector Double -> Double -> Vector Logical = "=="
infix non 4 (==) : Double -> Vector Double -> Vector Logical = "=="
infix non 4 (==) : Vector Character -> Vector Character -> Vector Logical = "=="
infix non 4 (==) : Vector Character -> Character -> Vector Logical = "=="
infix non 4 (==) : Character -> Vector Character -> Vector Logical = "=="
infix non 4 (==) : Vector Logical -> Vector Logical -> Vector Logical = "=="
infix non 4 (==) : Vector Logical -> Logical -> Vector Logical = "=="
infix non 4 (==) : Logical -> Vector Logical -> Vector Logical = "=="

infix non 4 (!=) : Integer -> Integer -> Logical = "!="
infix non 4 (!=) : Double -> Double -> Logical = "!="
infix non 4 (!=) : Character -> Character -> Logical = "!="
infix non 4 (!=) : Logical -> Logical -> Logical = "!="
infix non 4 (!=) : Vector Integer -> Vector Integer -> Vector Logical = "!="
infix non 4 (!=) : Vector Integer -> Integer -> Vector Logical = "!="
infix non 4 (!=) : Integer -> Vector Integer -> Vector Logical = "!="
infix non 4 (!=) : Vector Double -> Vector Double -> Vector Logical = "!="
infix non 4 (!=) : Vector Double -> Double -> Vector Logical = "!="
infix non 4 (!=) : Double -> Vector Double -> Vector Logical = "!="
infix non 4 (!=) : Vector Character -> Vector Character -> Vector Logical = "!="
infix non 4 (!=) : Vector Character -> Character -> Vector Logical = "!="
infix non 4 (!=) : Character -> Vector Character -> Vector Logical = "!="
infix non 4 (!=) : Vector Logical -> Vector Logical -> Vector Logical = "!="
infix non 4 (!=) : Vector Logical -> Logical -> Vector Logical = "!="
infix non 4 (!=) : Logical -> Vector Logical -> Vector Logical = "!="

infix non 4 (>) : Integer -> Integer -> Logical = ">"
infix non 4 (>) : Double -> Double -> Logical = ">"
infix non 4 (>) : Character -> Character -> Logical = ">"
infix non 4 (>) : Vector Integer -> Vector Integer -> Vector Logical = ">"
infix non 4 (>) : Vector Integer -> Integer -> Vector Logical = ">"
infix non 4 (>) : Integer -> Vector Integer -> Vector Logical = ">"
infix non 4 (>) : Vector Double -> Vector Double -> Vector Logical = ">"
infix non 4 (>) : Vector Double -> Double -> Vector Logical = ">"
infix non 4 (>) : Double -> Vector Double -> Vector Logical = ">"
infix non 4 (>) : Vector Character -> Vector Character -> Vector Logical = ">"
infix non 4 (>) : Vector Character -> Character -> Vector Logical = ">"
infix non 4 (>) : Character -> Vector Character -> Vector Logical = ">"

infix non 4 (<) : Integer -> Integer -> Logical = "<"
infix non 4 (<) : Double -> Double -> Logical = "<"
infix non 4 (<) : Character -> Character -> Logical = "<"
infix non 4 (<) : Vector Integer -> Vector Integer -> Vector Logical = "<"
infix non 4 (<) : Vector Integer -> Integer -> Vector Logical = "<"
infix non 4 (<) : Integer -> Vector Integer -> Vector Logical = "<"
infix non 4 (<) : Vector Double -> Vector Double -> Vector Logical = "<"
infix non 4 (<) : Vector Double -> Double -> Vector Logical = "<"
infix non 4 (<) : Double -> Vector Double -> Vector Logical = "<"
infix non 4 (<) : Vector Character -> Vector Character -> Vector Logical = "<"
infix non 4 (<) : Vector Character -> Character -> Vector Logical = "<"
infix non 4 (<) : Character -> Vector Character -> Vector Logical = "<"

infix non 4 (>=) : Integer -> Integer -> Logical = ">="
infix non 4 (>=) : Double -> Double -> Logical = ">="
infix non 4 (>=) : Character -> Character -> Logical = ">="
infix non 4 (>=) : Vector Integer -> Vector Integer -> Vector Logical = ">="
infix non 4 (>=) : Vector Integer -> Integer -> Vector Logical = ">="
infix non 4 (>=) : Integer -> Vector Integer -> Vector Logical = ">="
infix non 4 (>=) : Vector Double -> Vector Double -> Vector Logical = ">="
infix non 4 (>=) : Vector Double -> Double -> Vector Logical = ">="
infix non 4 (>=) : Double -> Vector Double -> Vector Logical = ">="
infix non 4 (>=) : Vector Character -> Vector Character -> Vector Logical = ">="
infix non 4 (>=) : Vector Character -> Character -> Vector Logical = ">="
infix non 4 (>=) : Character -> Vector Character -> Vector Logical = ">="

infix non 4 (<=) : Integer -> Integer -> Logical = "<="
infix non 4 (<=) : Double -> Double -> Logical = "<="
infix non 4 (<=) : Character -> Character -> Logical = "<="
infix non 4 (<=) : Vector Integer -> Vector Integer -> Vector Logical = "<="
infix non 4 (<=) : Vector Integer -> Integer -> Vector Logical = "<="
infix non 4 (<=) : Integer -> Vector Integer -> Vector Logical = "<="
infix non 4 (<=) : Vector Double -> Vector Double -> Vector Logical = "<="
infix non 4 (<=) : Vector Double -> Double -> Vector Logical = "<="
infix non 4 (<=) : Double -> Vector Double -> Vector Logical = "<="
infix non 4 (<=) : Vector Character -> Vector Character -> Vector Logical = "<="
infix non 4 (<=) : Vector Character -> Character -> Vector Logical = "<="
infix non 4 (<=) : Character -> Vector Character -> Vector Logical = "<="

prefix 9 (-) : Integer -> Integer = "-"
prefix 9 (-) : Double -> Double = "-"
prefix 9 (-) : Vector Integer -> Vector Integer = "-"
prefix 9 (-) : Vector Double -> Vector Double = "-"

