-- Haskell fun stuff
-- <- unwrap IO
-- $ wrap around IO

find :: Integer -> [Integer] -> Integer -> Integer
find 0 _ expected = if expected == 0 then 1 else 0
find depth [] expected = 0
find depth (head : tail) expected  | x /= 0 = head * x
                                   | otherwise = find depth tail expected
                                   where x = find (depth - 1) tail (expected - head)

getInputFile :: IO [Integer]
getInputFile = do
    contents <- readFile "input"
    return $ map (read::String->Integer) $ lines contents

main = do
    inputEntries <- getInputFile
    let solution = show (find 3 inputEntries 2020)
    putStrLn ("Solution: " ++ solution)