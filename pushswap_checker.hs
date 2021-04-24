import System.Exit
import System.Environment

myAppend :: [a] -> [a] -> [a]
myAppend [] [] = []
myAppend (a:b) (x:xs) = a:myAppend b (x:xs)
myAppend [] (x:xs) = x:myAppend [] xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:b) = myAppend (myReverse b) [a]

myFst :: (a, b) -> a
myFst (a, b) = a

mySnd :: (a, b) -> b
mySnd (a, b) = b

sa :: [Int] -> [Int]
sa [] = []
sa (a:b:c) = (b:a:c)

sb :: [Int] -> [Int]
sb [] = []
sb (a:b:c) = (b:a:c)

ra :: [Int] -> [Int]
ra [] = []
ra lst@(a:b) = myReverse (addInList(myReverse(deleteInList (lst))) [a])

rb :: [Int] -> [Int]
rb [] = []
rb lst@(a:b) = myReverse (addInList(myReverse(deleteInList (lst))) [a])

rra :: [Int] -> [Int]
rra [] = []
rra lst@(a:b) = (addInList (myReverse (deleteInList (myReverse lst))))
                [head (myReverse b)]

rrb :: [Int] -> [Int]
rrb [] = []
rrb lst@(a:b) = (addInList (myReverse (deleteInList (myReverse lst))))
                [head (myReverse b)]

isInt :: String -> Int ->Bool
isInt [] n = True
isInt ('-':y) 0 = isInt y 1
isInt (x:y) n  | x >= '0' && x <= '9' = isInt y (n + 1)
               | otherwise = False

readInt :: String -> [Int]
readInt [] = [-1]
readInt str | isInt str 0 = [(read str)]
            | otherwise = [-1]

tabToTabInt :: [String] -> [Int] -> IO ([Int])
tabToTabInt [] tab = return tab
tabToTabInt (x:y) tab | readInt x == [-1] = return [-1]
                      | otherwise = tabToTabInt (y) (tab ++ (readInt x))

strToTab :: String -> [String]
strToTab str = words str

checkTabR :: [String] -> Bool
checkTabR [] = True
checkTabR (x:y) = False

checkTab :: [String] -> Bool
checkTab [] = True
checkTab ("sa":y) = checkTab y
checkTab ("sb":y) = checkTab y
checkTab ("sc":y) = checkTab y
checkTab ("pa":y) = checkTab y
checkTab ("pb":y) = checkTab y
checkTab ("ra":y) = checkTab y
checkTab ("rb":y) = checkTab y
checkTab ("rr":y) = checkTab y
checkTab ("rra":y) = checkTab y
checkTab ("rrb":y) = checkTab y
checkTab ("rrr":y) = checkTab y
checkTab (x:y) = False

addInList :: [Int] -> [Int] -> [Int]
addInList lst [] = lst
addInList lst (x:xs) = [x] ++ lst

deleteInList :: [Int] -> [Int]
deleteInList [] = []
deleteInList (hd:[]) = []
deleteInList (hd:tl) = tl

isSort :: [Int] -> [Int] -> IO ()
isSort [] (lst) = putStrLn "KO"
isSort (x:[]) [] = putStrLn "OK"
isSort (x:[]) lst = putStrLn "KO"
isSort l@(x:xs) lst | x <= (head xs) = isSort xs lst
                  | otherwise = putStrLn "KO"


pushswapCheckR :: [String] -> [Int] -> [Int] -> ([Int], [Int])
pushswapCheckR str@(hd:tl) l1 l2
  | hd == "ra" = pushswapCheck tl (ra l1) l2
  | hd == "rb" = pushswapCheck tl l1 (rb l2)
  | hd == "rr" = pushswapCheck tl (ra l1) (rb l2)
  | hd == "rra" = pushswapCheck tl (rra l1) l2
  | hd == "rrb" = pushswapCheck tl l1 (rrb l2)
  | hd == "rrr" = pushswapCheck tl (rra l1) (rrb l2)


pushswapCheck :: [String] -> [Int] -> [Int] -> ([Int], [Int])
pushswapCheck [] l1 l2 = (l1, l2)
pushswapCheck str@(hd:tl) l1 l2
  | hd == "sa" = pushswapCheck tl (sa l1) l2
  | hd == "sb" = pushswapCheck tl l1 (sb l2)
  | hd == "sc" = pushswapCheck tl (sa l1) (sb l2)
  | hd == "pa" = pushswapCheck tl (addInList l1 l2) (deleteInList l2)
  | hd == "pb" = pushswapCheck tl (deleteInList l1) (addInList l2 l1)
  | otherwise = pushswapCheckR str l1 l2

main :: IO ()
main = do
  args <- getArgs
  cmd <- getLine
  tab_args <- tabToTabInt args []
  if (tab_args == [-1] || checkTab (strToTab cmd) == False)
    then
    exitWith(ExitFailure 84)
    else do
    let a = pushswapCheck (strToTab cmd) tab_args []
    isSort (myFst a) (mySnd a)
    exitWith(ExitSuccess)
