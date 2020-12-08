import Debug.Trace
import qualified Data.List as List
import Data.Maybe

type Instruction = (String, Int)
type Program = [Instruction]


getAccValue :: Program -> (Bool, Int)
getAccValue progr =
    let
        _getAccValue :: Int -> Int -> [Int] -> Instruction -> (Bool, Int)
        _getAccValue currentIndex acc visitedLines instr | List.elem currentIndex visitedLines = (False, acc)
                                                         | currentIndex == (length progr) - 1 = (True, if fst instr == "acc" then acc + (snd instr) else acc)
                                                         | fst instr == "acc" = _getAccValue (currentIndex + 1) (acc + (snd instr)) (visitedLines ++ [currentIndex]) (progr !! (currentIndex + 1))
                                                         | fst instr == "nop" = _getAccValue (currentIndex + 1) acc (visitedLines ++ [currentIndex]) (progr !! (currentIndex + 1))
                                                         | otherwise = _getAccValue (currentIndex + (snd instr)) acc (visitedLines ++ [currentIndex]) (progr !! (currentIndex + (snd instr)))
    in
        _getAccValue 0 0 [] (progr !! 0)


getNextIndex :: Program -> Int -> String -> Maybe Int
getNextIndex progr currentIndex currentFix = List.findIndex (\(v, _) -> if currentFix == "jmp" then v == "nop" else v == "jmp") $ drop (currentIndex) progr


changeOperation :: Program -> Int -> String -> Program
changeOperation progr currentIndex currentFix =
    let
        h = take currentIndex progr
        newValue = [(currentFix, snd (progr !! currentIndex))]
        t = drop (currentIndex + 1) progr
    in
        h ++ newValue ++ t

fixProgram :: Program -> (Bool, Int)
fixProgram progr =
    let
        _fixProgram :: Program -> Int -> String ->  (Bool, Int)
        _fixProgram curProgr currentIndex currentFix =
            let
                acc = getAccValue curProgr
                nextIndex = getNextIndex curProgr currentIndex currentFix
            in
                if (not $ fst $ acc) && (not $ isNothing nextIndex)then
                    _fixProgram (changeOperation progr (fromJust nextIndex + currentIndex) currentFix) (fromJust nextIndex + currentIndex + 1) currentFix
                else acc
    in
        let
            nopFix = _fixProgram progr 0 "jmp"
        in
            if fst nopFix then nopFix else _fixProgram progr 0 "nop"

parse :: [String] -> Program
parse lines =
    let
        _parse :: String -> Instruction
        _parse line =
            let
                lineSplit = words line
                instrValue = lineSplit !! 1
                sign = take 1 $ instrValue
            in
                (lineSplit !! 0, if sign == "+" then read $ drop 1 instrValue else ((-1) *) . read $ drop 1 instrValue)
    in
        [_parse line | line <- lines]

getInputFile :: IO Program
getInputFile = do
    contents <- readFile "input"
    return $ parse $ lines contents

main = do
    program <- getInputFile
    -- let accValue = getAccValue program
    -- putStrLn . show $ accValue

    let fixedAcc = fixProgram program
    putStrLn . show $ fixedAcc

    return ()