-- Haskell fun stuff
import Data.List
import Data.Maybe

-- Utils

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

findString :: String -> String -> Int
findString search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

hasElement :: (Eq a) => a -> [a] -> Bool
hasElement _ [] = False
hasElement x (head : tail) = if x == head then True else hasElement x tail


-- Business

isValidHairColor :: String -> Bool
isValidHairColor hairColor =
    let
        withoutHash = drop 1 hairColor
        validColors = ['a'..'f'] ++ ['0'..'9']
    in
        (hairColor !! 0 == '#') && (length withoutHash == 6) && (all (\c -> hasElement c validColors) withoutHash) 

isValidHeight :: String -> Bool
isValidHeight height =
    let
        isCm = findString "cm" height /= -1
        verifyHeight unit predicate =
            let
                unitIndex = findString unit height
                heightValue = if unitIndex /= -1 then read (take unitIndex height) :: Int else -1
            in
                predicate heightValue
    in
        if isCm then
            verifyHeight "cm" (\h -> h >= 150 && h <= 193)
        else
            verifyHeight "in" (\h -> h >= 59 && h <= 76)

isValidPassport :: String -> Bool
isValidPassport passport =
    let
        requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] -- cid is optional
        verifyField field =
            let
                fieldIndex = findString field passport
                fieldExists = fieldIndex /= -1
            in
                fieldExists &&
                let
                    fieldValue = head (wordsWhen (==' ') (drop (fieldIndex + 4) passport)) -- + 4 because the field name is always 3 characters and there is a `:`

                    isFieldValid = case field of
                        "byr" -> let byrValue = read fieldValue :: Int in byrValue >= 1920 && byrValue <= 2002
                        "iyr" -> let iyrValue = read fieldValue :: Int in iyrValue >= 2010 && iyrValue <= 2020
                        "eyr" -> let eyrValue = read fieldValue :: Int in eyrValue >= 2020 && eyrValue <= 2030
                        "hgt" -> isValidHeight fieldValue
                        "hcl" -> isValidHairColor fieldValue
                        "ecl" -> any (fieldValue==) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
                        "pid" -> length fieldValue == 9
                        _ -> False
                in
                    isFieldValid
    in
        all verifyField requiredFields

countValidPassports :: [String] -> Int
countValidPassports passports = length (filter isValidPassport passports)

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
    paragraphs <- getInputFile
    let validPassports = countValidPassports paragraphs
    putStrLn (show validPassports)
    return ()