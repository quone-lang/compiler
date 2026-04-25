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
#' Summary marker values interpreted by dataframe summarization.
type SummaryValue <- CountRows

#' Number of rows in the current summary group.
count : SummaryValue
count <- CountRows

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
extern elementwise map : (a -> b) -> Vector a -> Vector b = "purrr::map" { dispatch_on = "b" }
#' Apply a binary function to two vectors element by element.
extern elementwise map2 : (a -> b -> c) -> Vector a -> Vector b -> Vector c = "purrr::map2"

#' Count the elements in a vector.
extern reducer length : Vector a -> Integer = "length"
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
extern elementwise if_else : Logical -> a -> a -> a = "dplyr::if_else"

#' Motor Trend car road tests.
extern mtcars :
    dataframe
        { mpg : Vector Double
        , cyl : Vector Integer
        , disp : Vector Double
        , hp : Vector Double
        , drat : Vector Double
        , wt : Vector Double
        , qsec : Vector Double
        , vs : Vector Integer
        , am : Vector Integer
        , gear : Vector Integer
        , carb : Vector Integer
        } = "datasets::mtcars"

#' Fisher iris measurements with Quone-safe column names.
extern iris :
    dataframe
        { sepal_length : Vector Double
        , sepal_width : Vector Double
        , petal_length : Vector Double
        , petal_width : Vector Double
        , species : Vector Character
        } = "dplyr::mutate(dplyr::rename(datasets::iris, sepal_length = Sepal.Length, sepal_width = Sepal.Width, petal_length = Petal.Length, petal_width = Petal.Width, species = Species), species = as.character(species))"

#' New York air quality measurements with Quone-safe column names.
extern airquality :
    dataframe
        { ozone : Vector Double
        , solar_r : Vector Double
        , wind : Vector Double
        , temp : Vector Double
        , month : Vector Integer
        , day : Vector Integer
        } = "dplyr::rename(datasets::airquality, ozone = Ozone, solar_r = Solar.R, wind = Wind, temp = Temp, month = Month, day = Day)"

#' Plant growth experiment with Quone-safe column names.
extern plant_growth :
    dataframe
        { weight : Vector Double
        , group : Vector Character
        } = "dplyr::mutate(datasets::PlantGrowth, group = as.character(group))"

#' Tooth growth experiment with Quone-safe column names.
extern tooth_growth :
    dataframe
        { len : Vector Double
        , supp : Vector Character
        , dose : Vector Double
        } = "dplyr::mutate(datasets::ToothGrowth, supp = as.character(supp))"

#' Test whether a string contains a pattern.
extern elementwise str_detect : Character -> Character -> Logical = "stringr::str_detect"
#' Convert text to upper case.
extern elementwise str_to_upper : Character -> Character = "stringr::str_to_upper"
#' Convert text to lower case.
extern elementwise str_to_lower : Character -> Character = "stringr::str_to_lower"

#' Extract a Maybe value, using the fallback when it is Nothing.
with_default : a -> Maybe a -> a
with_default fallback m <-
    case m of
        Just x -> x
        Nothing -> fallback

#' Map over the Ok value of a Result, leaving Err unchanged.
result_map : (v -> w) -> Result e v -> Result e w
result_map f r <-
    case r of
        Ok v -> Ok (f v)
        Err e -> Err e

#' Stop execution with an error message.
extern reducer panic : Character -> a = "stop"

#' Unwrap an Ok value or stop execution on Err.
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

