-- Haskell fun stuff
-- (\x -> x + 1) is a lambda

-- Data
data Password = Password { letter :: Char, atLeast :: Int, atMost :: Int, password :: String } deriving (Show)


-- Business

xor :: Bool -> Bool -> Bool
xor a b = a /= b

isValidPassword :: Password -> Bool
isValidPassword password =
    let
        numberOfLetters = length $ filter (== letter(password)) (Main.password(password))
    in
        numberOfLetters >= atLeast(password) && numberOfLetters <= atMost(password)

-- For part 2, atLeast and atMost represent positions, letter must be at position atLeast but not in atMost
isValidPasswordPartTwo :: Password -> Bool
isValidPasswordPartTwo password =
    let
        letter = Main.letter(password)
    in
       xor (Main.password(password) !! (atLeast(password) - 1) == letter) (Main.password(password) !! (atMost(password) - 1) == letter)

getValidPasswordsNumber :: [Password] -> Int
getValidPasswordsNumber passwords = length (filter (isValidPasswordPartTwo) passwords)


-- Parsing stuff

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parse :: String -> Password
parse line =
    let
        splitBySpaces = wordsWhen (==' ') line
        ranges = wordsWhen (=='-') (head splitBySpaces)
        atLeast = read (head ranges)
        atMost = read (last ranges)
        letter = head (head (wordsWhen (==':') (head (tail splitBySpaces))))
        password = last splitBySpaces
    in
        Password letter atLeast atMost password


getInputFile :: IO [Password]
getInputFile = do
    contents <- readFile "input"
    return $ map (parse) $ (lines contents)

main = do
    passwords <- getInputFile
    let result = getValidPasswordsNumber passwords
    putStrLn (show result)
    return ()