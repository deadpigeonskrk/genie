type Person   = String
type Question = String

-- In our tree first subtree is yes, the second subtree is no
data QA = P Person | Q Question QA QA
    deriving (Read,Show)



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


defaultData :: QA
defaultData = Q "Is this person from Europe?" 
    (Q "Is this person a scientist?" (P "Marie Skłodowska-Curie") (P "Queen Elisabeth II")) 
    (Q "Is this person an actor?" (P "Marlin Monroe") (P "Hillary Clinton"))

beta :: QA -> IO()
beta (Q q y n) = do
                putStrLn q
                answ <- getLine 
                checkAn answ
                where checkAn a 
                                |yesAnswer == a = beta y
                                |noAnswer  == a = beta n
                                |otherwise      = error "I recognise only 'yes' or 'no' answer"
beta (P p)     = do
                 putStrLn $ "My guess: Is it " ++ p ++ "?"
                 answ <- getLine
                 checkAn answ
                where checkAn a 
                                |yesAnswer == a = putStrLn "I won"
                                |noAnswer  == a = putStrLn "You won"
                                |otherwise      = error "I recognise only 'yes' or 'no' answer"
 