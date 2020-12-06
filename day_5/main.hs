-- Haskell fun stuff

halfRoundUp :: Int -> Int
halfRoundUp x | x `mod` 2 == 0 = x `div` 2
                | otherwise = (x + 1) `div` 2

getHighestSeatId :: [String] -> Int
getHighestSeatId seats =
    let
        searchRow :: String -> Int -> Int -> Int
        searchRow seat a b | (head seat) == 'F' = searchRow (drop 1 seat) a (b - (halfRoundUp (b - a)))
                           | (head seat) == 'B' = searchRow (drop 1 seat) (a + (halfRoundUp (b - a))) b
                           | otherwise = min a b

        searchCol :: String -> Int -> Int -> Int
        searchCol [] a b = max a b
        searchCol seat a b | (head seat) == 'R' = searchCol (drop 1 seat) (a + (halfRoundUp (b - a))) b
                           | (head seat) == 'L' = searchCol (drop 1 seat) a (b - (halfRoundUp (b - a)))
    in
        maximum (map (\s -> (searchRow s 0 127) * 8 + (searchCol (drop 7 s) 0 7)) seats)


-- Parsing stuff

getInputFile :: IO [String]
getInputFile = do
    contents <- readFile "input"
    return $ lines contents

main = do
    input <- getInputFile
    let result = getHighestSeatId input
    putStrLn (show result)
    return ()