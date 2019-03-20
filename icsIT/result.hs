import Data.List

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

threeBiggestWords :: String -> [String]
threeBiggestWords xs = take 3 $ reverse $ sortOn length $ words xs

listPrint :: Tree String -> String -> [(String,[String])] -> [(String,[String])]
listPrint (Leaf xs) parent output = [(parent, threeBiggestWords xs)]
listPrint (Node l r) parent output = (listPrint l (parent ++ "L") output) ++ (listPrint r (parent ++ "R") output)

printResult tree = listPrint tree "P" [("P",[""])]

{- Next lines is a TREEs for test
    USAGE: 
        install Haskell project via https://www.haskell.org/downloads/#platform
        open WinGhci - 
        Load *.hs file
        use test TREEs for verifying programm: "printResult test" "printResult test1" or so on.
-}

test = Node (Node (Leaf "Test1 test12 test tes te") (Leaf "This is gonna be interesting"))(Leaf "")
test1 = Leaf "This string is a test stuff"
test2 = Node (Node (Node(Leaf "Leviathan is a good movie")(Leaf "Im the right branch of the node"))(Leaf ""))(Node (Leaf "Test! I love haskell")(Leaf "Too old for this amazing stuff"))