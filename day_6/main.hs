-- Haskell fun stuff

removeDuplicates :: String -> String
removeDuplicates [] = []
removeDuplicates (' ' : tail) = ' ' : removeDuplicates tail
removeDuplicates (head  : tail) = head : removeDuplicates (map mark tail) where mark y = if head == y then ' ' else y

countYesVotes :: [String] -> Int
countYesVotes votesPerGroup =
    let
        countVotes :: String -> Int
        countVotes votes = length (filter (/=' ') (removeDuplicates votes))
    in
        foldr (+) 0 (map (\v -> countVotes v) votesPerGroup)
    

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

    return ()