module Lab5
where 
import System.IO
import System.IO.Error(tryIOError)


-- | Data type
type Person   = String
type Question = String

-- In our tree first subtree is yes, the second subtree is no
data QA = P Person | Q Question QA QA
    deriving (Read,Show)

-- | Utilities 
yesAnswer :: String
yesAnswer = "yes"
noAnswer :: String
noAnswer = "no"

question :: String -> QA -> QA -> QA 
question q yesTree noTree = Q q yesTree noTree

guess :: String -> QA
guess gs = P gs

writeQFile :: QA -> IO ()
writeQFile qa = writeFile "questions.qa" (show qa)


-- | Default data
defaultData :: QA
defaultData = Q "Is this person from Europe?" 
    (Q "Is this person a scientist?" 
        (P "Marie Skłodowska-Curie") 
        (P "Queen Elisabeth II")) 
    (Q "Is this person an actor?" 
        (P "Marilyn Monroe") 
        (P "Hillary Clinton"))


-- | Ask questions recursively until program has a guess 
-- | returns person guessed & answer
askQ :: QA -> IO (String, String)
askQ (Q q y n) = do
                    putStrLn q
                    answ <- getLine 
                    checkAn answ
                    where checkAn a 
                            | yesAnswer == a = askQ y
                            | noAnswer  == a = askQ n
                            | otherwise      = do
                                        putStrLn "I recognise only 'yes' or 'no' answer"
                                        askQ (Q q y n)
askQ (P p)     = do
                putStrLn $ "My guess: Is it " ++ p ++ "?"
                answ <- getLine
                checkAn answ
                where checkAn a 
                        | yesAnswer /= a && noAnswer /= a = 
                            putStrLn "I recognise only 'yes' or 'no' answer" >> askQ (P p)
                        | otherwise = return (p, a)
 
main :: IO ()
main = do
    questions <- getData
    result <- askQ questions
    updatedquestions <- checkAns result questions
    writeQFile questions
    where checkAns (lastGuess, answer) questions 
                    | yesAnswer == answer = 
                       putStrLn "I won" 
                       >> return playAgain
                    | noAnswer  == answer =
                       putStrLn "OK - you won this time." 
                       >> return (addInfo questions lastGuess)

playAgain = defaultData

addInfo :: QA -> Person -> QA
addInfo = undefined

getData :: IO QA
getData = return defaultData