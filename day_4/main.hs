-- Haskell fun stuff
import Data.List
import Data.Maybe

findString :: String -> String -> Int
findString search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

isValidPassport :: String -> Bool
isValidPassport passport =
    let
        requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] -- cid is optional
    in
        all (\x -> findString x passport /= -1) requiredFields

countValidPassports :: [String] -> Int
countValidPassports passports = length (filter isValidPassport passports)

-- Parsing stuff

getParagraphs :: [String] -> [String]
getParagraphs lines =
    let
        getParagraphsHelper :: [String] -> [String] -> String -> [String]
        getParagraphsHelper [] paragraphs paragraph = paragraphs ++ [paragraph]
        getParagraphsHelper ("" : tail) paragraphs paragraph = getParagraphsHelper tail (paragraphs ++ [paragraph]) ""
        getParagraphsHelper (head : tail) paragraphs paragraph = getParagraphsHelper tail paragraphs (paragraph ++ head)
    in
        getParagraphsHelper lines [] ""

getInputFile :: IO [String]
getInputFile = do
    contents <- readFile "input"
    return $ (getParagraphs (lines contents))

main = do
    paragraphs <- getInputFile
    let validPassports = countValidPassports paragraphs
    putStrLn (show validPassports)
    return ()