import Debug.Trace
-- Haskell fun stuff



-- Data
data Map = Map { _map :: [String], width :: Int, height :: Int } deriving (Show)

-- Business
isTree :: Map -> Int -> Int -> Bool
isTree map x y =
    let
        currentX = x `mod` width map
        currentY = y `mod` height map
    in
        _map map !! currentY !! currentX == '#'


countAllTrees :: Map -> Int
countAllTrees map =
    let
        countRec x y treeSum
            | y == height map = treeSum
            | otherwise = if isTree map x y then countRec (x + 3) (y + 1) (treeSum + 1) else countRec (x + 3) (y + 1) treeSum
    in
        countRec 0 0 0

-- Parsing stuff

getInputFile :: IO Map 
getInputFile = do
    contents <- readFile "input"
    let mapLines = lines contents
    return $ Map mapLines (length (head mapLines)) (length mapLines)

main = do
    map <- getInputFile
    let result = countAllTrees map
    putStrLn (show result)
    return ()