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


countAllTrees :: Map -> Int -> Int -> Int
countAllTrees map xOffset yOffset =
    let
        countRec x y treeSum
            | y >= height map = treeSum
            | otherwise = if isTree map x y then
                            countRec (x + xOffset) (y + yOffset) (treeSum + 1)
                          else
                            countRec (x + xOffset) (y + yOffset) treeSum
    in
        countRec 0 0 (if isTree map 0 0 then 1 else 0)

-- Parsing stuff

getInputFile :: IO Map 
getInputFile = do
    contents <- readFile "input"
    let mapLines = lines contents
    return $ Map mapLines (length (head mapLines)) (length mapLines)

main = do
    map <- getInputFile
    let firstResult = countAllTrees map 3 1
    putStrLn (show firstResult)
    let result = product [countAllTrees map 3 1, countAllTrees map 1 1, countAllTrees map 5 1, countAllTrees map 7 1, countAllTrees map 1 2]
    putStrLn (show result)
    return ()