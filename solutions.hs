import Data.List

data NestedList a = Elem a | List [NestedList a]

data Quantify a = Single a | Multiple Int a
                    deriving (Show)

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

-- Problem 11
encodeModified :: Eq a => [a] -> [Quantify a]
encodeModified = map (\x -> if (length x) > 1 then Multiple (length x) (head x) else Single (head x)) . Data.List.group

-- Problem 12
decodeModified :: [Quantify a] -> [a]
decodeModified = concatMap decodeModified'
                  where decodeModified' (Single a) = [a]
                        decodeModified' (Multiple times a) = replicate times a

-- Problem 13
-- Not sure what the problem means, skipping fro now.

-- Problem 14
dupli :: [a] -> [a]
dupli = concatMap (replicate 2)


-- Problem 15
repli :: [a] -> Int -> [a]
repli xs times = concatMap (replicate times) xs


-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs pos = loopWithIdx xs 1
                    where loopWithIdx [] _ = []
                          loopWithIdx (x:xs) idx
                            | idx `mod` pos == 0 = loopWithIdx xs (idx + 1)
                            | otherwise          =  x : loopWithIdx xs (idx + 1)

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split xs pos = ((take pos xs), (drop pos xs))

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs start end = take (end - start + 1) $ drop (start - 1) xs 

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate [] _ = []
rotate xs pos
  | pos < 0   = rotateHelper xs (len + pos)
  | otherwise = rotateHelper xs pos
  where len = length xs
        rotateHelper xs pos = drop pos xs ++ take pos xs

-- Problem 20
removeAt :: Int -> [a] -> [a]


