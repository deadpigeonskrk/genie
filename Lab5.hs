{-
Group: 1
Authors: Stanisław Lorys, Caroline Grand-Clement
-}

module Lab5
where 
import System.IO
import System.IO.Error(tryIOError)
import Text.Read
import Data.Maybe

-- | Data type
type Person   = String
type Question = String

-- | In our tree first subtree is yes, the second subtree is no
data QA = P Person | Q Question QA QA
    deriving (Read,Show)

-- | Utilities 
yesAnswer :: String
yesAnswer = "yes"
noAnswer :: String
noAnswer = "no"

isYes, isNo :: String -> Bool
isYes ans = ans == yesAnswer
isNo ans = ans == noAnswer

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

---- | MAIN EXECUTION

-- | Retrieve data
getData :: IO QA
getData = readcontent <$> tryIOError (readFile "questions.qa")

-- | Extract content from file if found or valid
-- | otherwise returns default data
readcontent :: Either IOError String -> QA
readcontent (Left _)    = defaultData
readcontent (Right str) = getQA $ readMaybe str
                        where getQA (Just qa) = qa
                              getQA Nothing = defaultData

-- | Ask questions recursively until program has a guess 
-- | returns person guessed & answer
askQ :: QA -> IO (String, String)
askQ (Q q y n) = 
    do
        putStrLn q
        answ <- getLine 
        checkAns answ
        where checkAns a 
                | isYes a = askQ y
                | isNo  a = askQ n
                | otherwise      = do
                            putStrLn "I recognise only 'yes' or 'no' answer"
                            askQ (Q q y n)
askQ (P p)     = 
    do
        putStrLn $ "My guess: Is it " ++ p ++ "?"
        answ <- getLine
        checkAns answ
        where checkAns a 
                | (not . isYes) a && (not . isNo) a = 
                    putStrLn "I recognise only 'yes' or 'no' answer" 
                    >> askQ (P p)
                | otherwise = return (p, a)

-- | Main game loop
playGame :: QA -> IO QA
playGame questions = do
    result <- askQ questions              -- get results from questions/guess
    updated <- checkAns result questions  -- updates question tree if needed
    rep <- playAgain                      -- checks if user wants to play again
    again updated rep
    where checkAns (lastGuess, answer) questions 
                | isYes answer = do
                   putStrLn "I won" 
                   return questions
                | isNo answer = do
                   putStrLn ("OK - you won this time." 
                     ++ "\n Just curious: Who was your famous person?")
                   newPerson <- getLine
                   putStrLn ("Give me a question for which the answer for "
                     ++ newPerson ++  " is 'yes' and the answer for "
                     ++ lastGuess ++ " is 'no'.")
                   newQuest <- getLine
                   return (addInfo questions lastGuess newPerson newQuest)
          again questions True   = playGame questions
          again questions False  = return questions

-- | Ask player if they want to play again
playAgain :: IO Bool
playAgain = do
    putStr "Play again? "
    ans <- getLine
    return (isYes ans)

-- | Update question tree 
addInfo :: QA -> Person -> Person -> Question -> QA
addInfo (Q q y n) wrongPer newPer newQ = Q q 
                                (addInfo y wrongPer newPer newQ) 
                                (addInfo n wrongPer newPer newQ)
addInfo (P p)     wrongPer newPer newQ
                            | p == wrongPer = Q newQ (P newPer) (P wrongPer)
                            | otherwise     = P p

-- | Main program - retrieves data, plays game, saves data when done
main :: IO ()
main = do
    questions <- getData
    updatedQuestions <- playGame questions
    writeQFile updatedQuestions