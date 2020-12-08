-- Haskell fun stuff
import Debug.Trace
import Data.String
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char

type Bags = Map.Map String [(Int, String)]


hasShinyGold :: String -> Bags -> Set.Set (String) -> (Bool, Set.Set (String))
hasShinyGold currentBag bags set | currentBag == "shiny gold" = (True, set)
                                 | Set.member currentBag set = (True, set)
                                 | (Map.findWithDefault [] currentBag bags) == [] = (False, set)
                                 | otherwise = foldl (\acc k -> if (fst $ hasShinyGold (snd k) bags (snd acc)) then (True, Set.insert currentBag (snd acc)) else (fst acc, snd acc)) (False, set) $ Map.findWithDefault [] currentBag bags

countBagsContainingShinyGold :: Bags -> Int
countBagsContainingShinyGold bags =
    let
        bagsSet = Set.empty
    in
        (length $ foldl (\acc k -> if (fst $ hasShinyGold k bags acc) then Set.insert k acc else acc) Set.empty $ filter (/= "shiny gold") $ Map.keys bags)

countBagsInShinyGold :: String -> Bags -> Int
countBagsInShinyGold currentBag bags | (Map.findWithDefault [] currentBag bags) == [] = 1
                                     | otherwise = foldl (\acc b -> acc + fst b * countBagsInShinyGold (snd b) bags) 1 $ Map.findWithDefault [] currentBag bags

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let
        (ys, zs) = splitAt n xs
    in
        ys : chunks n zs

parse :: [String] -> Bags
parse lines =
    let
        parseLine :: String -> (String, [(Int, String)])
        parseLine line =
            let
                lineSplited = words line
                bagName = lineSplited !! 0 ++ " " ++ lineSplited !! 1
                isNo = lineSplited !! 4 == "no"
                bagsContained = chunks 4 $ drop 4 lineSplited
            in
                (bagName, if isNo then [] else map (\b -> (read $ b !! 0, b !! 1 ++ " " ++ b !! 2)) bagsContained)
    in
        Map.fromList $ map parseLine lines


getInputFile :: IO Bags
getInputFile = do
    contents <- readFile "input"
    return $ (parse $ lines contents)

main = do
    bags <- getInputFile
    let test =  countBagsContainingShinyGold bags
    putStrLn . show $ test

    let result = (countBagsInShinyGold "shiny gold" bags) - 1 -- Remove shiny gold
    putStrLn . show $ result

    return ()