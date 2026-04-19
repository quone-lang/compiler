m <- Just 42

unwrap <- case m of
    Just n -> n
    Nothing -> 0

threshold <- if 5 > 3 then "high" else "low"
