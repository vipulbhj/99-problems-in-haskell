import Data.List

data NestedList a = Elem a | List [NestedList a]


-- Problem 01
myLast []   = error "Are you dumb ..??"
myLast [x]  = x
myLast (_:xs) = myLast xs

-- Problem 02
mySecondLast [] = error "Yoo, you dumb...?"
mySecondLast [x] = error "Error .. ?"
mySecondLast [x, _] = x
mySecondLast (_:xs) = mySecondLast xs

-- Problem 03
elementAt xs pos
  | pos <= 0 = error "Error.."
  | pos == 1 = head xs
  | otherwise = elementAt (tail xs) (pos - 1)

-- Problem 04
myLength xs = myLength' xs 0
  where  myLength' [] len = len
         myLength' (_:xs) len = myLength' xs (len + 1)


-- Problem 05
myReverse xs = myReverse' xs []
  where  myReverse' [] ys = ys
         myReverse' (x:xs) ys = myReverse' xs (x:ys)

-- Problem 06
isPalindrome xs = (myReverse xs) == xs

-- Problem 07
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List x) = concatMap flatten x

-- Problem 08
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:[]) = x:[]
compress (x:y:rest) = if x == y then compress (x:rest) else x:compress (y:rest)

-- Problem 09
pack :: Eq a => [a] -> [[a]]
pack = Data.List.group

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . Data.List.group
