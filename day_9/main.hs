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
        


getRange :: Int -> [Int] -> Int
getRange invalidNumber numbers =
    let
        _getRange :: Int -> [Int] -> [Int] -> Int 
        _getRange acc (head : tail) range | acc + head == invalidNumber = minimum (head : range) + maximum (head : range)
                                          | acc + head > invalidNumber = _getRange (acc - last range) (head : tail) (init range)
                                          | otherwise = _getRange (acc + head) tail (head : range)

    in
        _getRange 0 numbers []

getInputFile :: IO [Int]
getInputFile = do
    contents <- readFile "input"
    return $ [read line | line <- lines contents]

main = do
    content <- getInputFile

    let invalidNumber = getFirstInvalidNumber content
    putStrLn . show $ invalidNumber

    let numberRange = getRange invalidNumber content
    putStrLn . show $ numberRange

    return ()