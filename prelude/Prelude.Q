#' The Quone standard prelude.
#'
#' This file is implicitly imported into every user module. It defines the
#' initial-release primitive types, core custom types, fixed operator overloads,
#' common vectorized functions, reducers, and small Result helpers.

#' Whole-number scalar type. Integer literals use the `L` suffix.
extern type Integer
#' Floating-point scalar type. Bare numeric literals such as `1` are Double.
extern type Double
#' Text scalar type. Lowers to R character values.
extern type Character
#' Homogeneous vector type.
extern type Vector a

#' Logical truth value.
type Logical <- True | False
#' Optional value. Use `Nothing` for missingness and `Just value` for present data.
type Maybe a <- Nothing | Just a
#' Fallible result. `Ok value` is success; `Err error` is failure.
type Result e a <- Err e | Ok a

#' Convert an Integer to a Double explicitly.
extern elementwise to_double : Integer -> Double = "as.double"
#' Convert a value to Character using R's `as.character`.
extern elementwise to_character : a -> Character = "as.character"

#' Square root.
extern elementwise sqrt : Double -> Double = "sqrt"
#' Absolute value.
extern elementwise abs : Double -> Double = "abs"
#' Natural logarithm.
extern elementwise log : Double -> Double = "log"
#' Exponential function.
extern elementwise exp : Double -> Double = "exp"
#' Round to the nearest whole value.
extern elementwise round : Double -> Double = "round"
#' Round down.
extern elementwise floor : Double -> Double = "floor"
#' Round up.
extern elementwise ceiling : Double -> Double = "ceiling"

#' Apply a function to every element of a vector.
#'
#' Arguments:
#' - f: function to apply
#' - xs: input vector
extern elementwise map : (a -> b) -> Vector a -> Vector b = "purrr::map" { dispatch_on = "b" }
#' Apply a binary function to two vectors element by element.
#'
#' Arguments:
#' - f: function to apply
#' - xs: first input vector
#' - ys: second input vector
extern elementwise map2 : (a -> b -> c) -> Vector a -> Vector b -> Vector c = "purrr::map2"

#' Count the elements in a vector.
extern reducer length : Vector a -> Integer = "length"
#' Count the elements in a vector.
extern reducer count : Vector a -> Integer = "length"
#' Arithmetic mean.
extern reducer mean : Vector Double -> Double = "mean"
#' Median.
extern reducer median : Vector Double -> Double = "stats::median"
#' Sum of a numeric vector.
extern reducer sum : Vector Double -> Double = "sum"
#' Standard deviation.
extern reducer sd : Vector Double -> Double = "stats::sd"
#' Variance.
extern reducer var : Vector Double -> Double = "stats::var"
#' Minimum value.
extern reducer min : Vector Double -> Double = "min"
#' Maximum value.
extern reducer max : Vector Double -> Double = "max"

#' Vectorized conditional. Use when the condition may be a vector.
#'
#' Arguments:
#' - condition: logical condition
#' - yes: value when condition is True
#' - no: value when condition is False
extern elementwise if_else : Logical -> a -> a -> a = "dplyr::if_else"

#' Test whether a string contains a pattern.
#'
#' Arguments:
#' - pattern: text pattern to find
#' - string: text to search
extern elementwise str_detect : Character -> Character -> Logical = "stringr::str_detect"
#' Convert text to upper case.
extern elementwise str_to_upper : Character -> Character = "stringr::str_to_upper"
#' Convert text to lower case.
extern elementwise str_to_lower : Character -> Character = "stringr::str_to_lower"

#' Extract a Maybe value, using the fallback when it is Nothing.
#'
#' Arguments:
#' - fallback: value to use for Nothing
#' - maybe: optional value to unwrap
with_default : a -> Maybe a -> a
with_default fallback m <-
    case m of
        Just x -> x
        Nothing -> fallback

#' Map over the Ok value of a Result, leaving Err unchanged.
#'
#' Arguments:
#' - f: function to apply to the Ok value
#' - result: result to transform
result_map : (v -> w) -> Result e v -> Result e w
result_map f r <-
    case r of
        Ok v -> Ok (f v)
        Err e -> Err e

#' Stop execution with an error message.
#'
#' Arguments:
#' - message: error message
extern reducer panic : Character -> a = "stop"

#' Unwrap an Ok value or stop execution on Err.
#'
#' Arguments:
#' - result: fallible value to unwrap
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

