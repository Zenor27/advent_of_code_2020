import Debug.Trace

hasSumInList :: Int -> [Int] -> Bool
hasSumInList valueToFind values = any (==True) [True | a <- values, b <- values, a /= b, a + b == valueToFind]

getFirstInvalidNumber :: [Int] -> Int
getFirstInvalidNumber numbers =
    let
        preambuleSize = 25
        values = drop preambuleSize numbers
        preambule = take preambuleSize numbers
        _findInvalidNumber :: [Int] -> [Int] -> Int
        _findInvalidNumber [] _ = error "list should not be empty..."
        _findInvalidNumber (valueHead : valueTail) currentPreambule  | hasSumInList valueHead currentPreambule = _findInvalidNumber valueTail ((drop 1 currentPreambule) ++ [valueHead])
                                                                     | otherwise = valueHead

    in
        _findInvalidNumber values preambule
        


getInputFile :: IO [Int]
getInputFile = do
    contents <- readFile "input"
    return $ [read line | line <- lines contents]

main = do
    content <- getInputFile

    let result = getFirstInvalidNumber content
    putStrLn . show $ result

    return ()