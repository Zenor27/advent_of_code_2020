-- Haskell fun stuff
import Data.List

removeDuplicates :: String -> String
removeDuplicates [] = []
removeDuplicates ('_' : tail) = '_' : removeDuplicates tail
removeDuplicates (head  : tail) = head : removeDuplicates (map mark tail) where mark y = if head == y then '_' else y

-- Not my favourite code here...

countYesVotes :: [String] -> Int
countYesVotes votesPerGroup =
    let
        countVotes :: String -> Int
        countVotes votes = length (filter (\c -> c /= ' ' && c /= '_') (removeDuplicates votes))
    in
        foldr (+) 0 (map (\v -> countVotes v) votesPerGroup)
    
countMajorities :: [String] -> Int
countMajorities votesPerGroup =
    let
        countMajority :: String -> Int
        countMajority votes =
            let
                nbPersons = length (filter (==' ') votes)
                votesWithoutSpaces = filter (/= ' ') votes
                votesWithMajority =  length (filter (\nbVotes -> nbVotes == nbPersons) (map (\v -> length v) $ group $ sort votesWithoutSpaces))
            in
                votesWithMajority
    in
        foldr (+) 0 (map countMajority votesPerGroup)

-- Parsing stuff

getParagraphs :: [String] -> [String]
getParagraphs lines =
    let
        getParagraphsHelper :: [String] -> [String] -> String -> [String]
        getParagraphsHelper [] paragraphs paragraph = paragraphs ++ [paragraph]
        getParagraphsHelper ("" : tail) paragraphs paragraph = getParagraphsHelper tail (paragraphs ++ [paragraph]) ""
        getParagraphsHelper (head : tail) paragraphs paragraph = getParagraphsHelper tail paragraphs (paragraph ++ head ++ " ")
    in
        getParagraphsHelper lines [] ""

getInputFile :: IO [String]
getInputFile = do
    contents <- readFile "input"
    return $ (getParagraphs (lines contents))

main = do
    input <- getInputFile
    let result = countYesVotes input
    putStrLn (show result)

    let majorities = countMajorities input
    putStrLn (show majorities)

    return ()