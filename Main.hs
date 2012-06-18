-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main ( main ) where
import Char
import Data.Maybe
import Data.List
import System.Environment
import System.Exit
import Random
import Control.Applicative

myLast  :: [a]  -> a
myLast (f:[])  = f
myLast (x:xs) = myLast(xs)



myButLast  :: [a]  -> a
myButLast (x:y:[])  = x
myButLast (x:xs) = myButLast(xs)



elementAt ::(Num i) =>  [a] -> i -> a
elementAt  (x:xs) 1 = x
elementAt  (x:xs) n = elementAt xs (n-1)



myLength :: ( Num i ) => [a] -> i
myLength [] = 0
myLength (x:xs) = 1  + myLength xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = (myreverse xs) ++ [x]


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome x = reverse x == x



data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x


compress :: (Eq a) => [a] -> [a]
compress ls = foldr (\x y  -> if  (x /= (head y)) then x:y else  y)  [last ls] ls


pack :: (Eq a) => [a] -> [[a]]
pack = foldr func []
    where func x []     = [[x]]
          func x (y:xs) =
              if x == (head y) then ((x:y):xs) else ([x]:y:xs)

encode ls = map (\x   -> (head x, length x) )   (pack ls)



data Runlength a = Single a | Multiple Int a deriving Show

modifiedEncode ::(Eq a) => [a] -> [Runlength a]
modifiedEncode ls = map ( \ x  -> if length x == 1 then Single (head x) else Multiple (length x) (head x) )   (pack  ls)




dupli :: [a] -> [a]
dupli ls  =  concatMap (\x  -> [x,x])   ls




repli ls  n = concatMap (\ x -> replicate n x) ls




--dropEvery :: (Eq a) => [a] -> Int -> [a]
--dropEvery ls n = foldl (\acc y -> if myElemIndex y ls `mod` n == 0 then acc else acc ++ [y] )  []   ls
--dropEvery ls n = foldl (\acc y -> if (length acc * n) `mod` n == 0 then acc else acc ++ [y] )  []  ls

dropEvery2 :: [a] -> Int -> ([a],Int)
dropEvery2 xs n =  foldr (\x (acc, i) -> (if mod i n == 0 then acc else x:acc, i - 1)) ([], length xs) xs


helper :: Int -> ([a],[a]) -> ([a],[a])
helper n (acc,[]) = (acc,[])
helper  n (acc,h:tail) = if length acc == n then (acc, h:tail)  else helper n (acc ++ [h], tail)

split :: [a] -> Int -> ([a],[a])
split list n = helper n ([], list)



slice :: [a] -> Int -> Int -> [a]
slice (h:tail) s e =  if s ==1 then take (e-s+1) (h:tail) else slice tail (s-1) e


rotateHelper :: [a] -> Int -> [a]
rotateHelper (h:tail) 0 = (h:tail)
rotateHelper (h:tail) n = rotateHelper (tail++[h]) (n-1)


rotate :: [a] -> Int -> [a]
rotate (h:tail) 0 = (h:tail)
rotate list n = if n > 0 then rotateHelper list n else rotateHelper list (length (list) +n)


removeAt :: [a] -> Int -> [a]
removeAt (h:tail) 0 =  tail
removeAt (h:tail) n = h : (removeAt tail (n-1))


insertAt :: a -> [a] -> Int -> [a]
--insertAt e list 1 = e : list
--insertAt e (h:tail) n = h : insertAt e tail (n-1)
insertAt e list n = foldl     (\acc x -> if length acc == n then acc ++ [e] ++ [x] else acc ++ [x] )   [] list



range :: Int -> Int -> [Int]
range s e = scanl (\start next -> start+next )  s   (replicate (e-s) 1)



randomList :: Int -> Int -> Int -> IO [Int]
randomList n a b = do
  g <- newStdGen
  return (take n $ randomRs (a,b) g)



--rnd_select :: [a] -> Int -> IO [a]
--rnd_select list n = do
--                    listOfRandoms <-  (randomList n 0 (length list -1))
--                    return $ map (\x -> list !! x) listOfRandoms



rnd_select :: [a] -> Int -> IO [a]
rnd_select list n = do
                    listOfRandoms <-  (randomList n 0 (length list -1))
                    return $ [ list !! p | p <- listOfRandoms ]



diff_select :: Int -> Int -> IO [Int]
diff_select n end = rnd_select   (range 1 end)  n


diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
  gen <- getStdGen
  return . take n $ randomRs (1, m) gen



rnd_permu :: [a] -> IO [a]
rnd_permu [] = return []
rnd_permu list = do
                    randomList <- randomList 1 0 (length list -1)
                    let random  = randomList !! 0
                    rest <- rnd_permu (removeAt  list random)
                    return $ (list !! random) : rest

deleteElem  elem = filter (\x -> x/= elem  )


--combinations :: [a] -> Int -> [[a]]
--combinations (x:[]) n = [[x]]
--combinations (x:(y:[])) n = [(x:y:[])]
----combinations list n = map (\elem -> (combinations ( list )  n) )  list
--combinations list n = map (\elem -> [elem] )  list


--combinations :: (Eq a ) => [a] -> Int -> [[a]]
--combinations list 1 = [list]
--combinations list n = if length list == n then [list] else combinations list n
--combinations list n = map (\elem -> concat (combinations (deleteElem elem list) (n-1)) )  list


combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)




deleteThese :: (Eq a) =>  [a] -> [a] -> ([a],[a])
deleteThese toDelete list = (list \\ (list \\ toDelete),  list \\ toDelete )

--mygroup ::(Ord a) => [a] -> [[a]]
--mygroup [[] = [[]]
--mygroup list = foldl (\two ->  (combinations 3 (snd (deleteThese two list)))) (combinations 2 list)  []
--mygroup list = map (\pair -> fst pair ++ (combinations 1 (snd pair)))   twoAndRest
--        where twoAndRest = zip  (combinations 2 list)   (mymap list)


mymap :: (Eq a) => [a] -> [[a]]
mymap list = map (\two -> list \\ two) ( combinations 1 list)


lsort :: [[a]] -> [[a]]
lsort listOfLists = sortBy (\lista listb -> if length lista < length listb then LT else GT)  listOfLists


lfSort :: [[a]] -> [[a]]
lfSort listOfLists = sortBy (\lista listb -> if (freq lista listOfLists) <  (freq listb listOfLists) then GT else LT  ) listOfLists


freq :: [a] -> [[a]] -> Int
freq list listOfLists = length(  filter (\x -> length list == x)  (map (\elem -> length elem) listOfLists))

wordCount input = show ((length input)) ++ "\n"

data List a = Cons a (List a)
            | Nil
              deriving (Show)


toList :: [a] -> List a
toList (x:xs) = Cons x  (toList xs)
toList [] = Nil



listMean :: [Int] -> Double
listMean list = fromIntegral (foldl1 (+ )  list) /   fromIntegral (length list)

main = do

        print $ listMean [1,2,3,6]


































































