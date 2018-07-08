import Data.Char

-- Chapter 1
e13_product [] = 1
e13_product (n:ns) = n * e13_product ns



-- Chapter 2
--    Examples
double x = x + x
quadruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns

--    Exercises
-- 1. (See above)
-- 2. 
--   a) (2^3)*4 = 32
--   b) 
-- 3.
n = a `div` length xs
   where
   a = 10
   xs = [1,2,3,4,5]

-- 4.
last2 xs = xs !! (length xs - 1)
last3 xs = head (reverse xs)

-- 5.
init2 xs = take (length xs - 1) xs
init3 xs = reverse (drop 1 (reverse xs))

-- Chapter 3
--    Examples
add :: (Int,Int) -> Int
add (x,y) = x + y
zerto n = [0..n]

add' :: Int -> (Int -> Int)
add' x y = x + y

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

--      Exercises
-- 1.
--   [Char]
--   (Char, Char, Char)
--   [(Bool,Char)]
--   ([Bool],[Char])
--   [[a] -> [a]]

-- 2. 
--   [True, True, False]
--   [[1,2,3,4][4,3,2,1]]
--   add x y z = a + b + c
--   copy x = (x,x)
--   apply e x = e x 

-- 3.
--   second :: [a] -> a
--   swap :: (a,b) -> (b,a)
--   pair :: a -> b -> (a,b)
--   double :: Num a => a -> a
--   panlindrome :: Eq a => [a] -> Bool
--   twice :: (a -> b) -> a -> b

-- 4. The functions would have to be tested for every possible input. If it takes a simple parameters (such as Bool) then this may be feasible but otherwise it would take too long 
--    to compare them.

-- Chapter 4
-- 	Exercises
-- 1.
halve1 :: [a] -> ([a],[a])
halve1 xs | length xs `mod` 2 /= 0 = (xs,[])
          | otherwise = (take halfLength xs, drop halfLength xs)
              where halfLength = length xs `div` 2

-- 2.
--   a.
third1 :: [a] -> a
third1 xs | length xs >=3  = head (tail (tail xs))
--   b.
third2 :: [a] -> a
third2 xs | length xs >=3 = xs !! 2
--   c.
third3 :: [a] -> a
third3 (_:_:x3:xs) = x3

-- 3.
--   a.
safetail1 :: [a] -> [a]
safetail1 as = if length as == 0 then as else tail as
--   b.
safetail2 :: [a] -> [a]
safetail2 as | length as == 0 = as
             | otherwise = tail as
--   c.
safetail3 :: [a] -> [a]
safetail3 (_:xs) = xs
safetail3 xs = xs

-- 4. show how || operator can be defined in four different ways using pattern matching.
{-
   type - (||) :: Bool -> Bool -> Bool
    a.
	True || True = True
	True || False = True
	False || True = True
	False || False = False 
    b.
	True || _ = True
	_ || True = True
    c.
	False || False = False
	_ || _ = True
    d.
	False || b = b
	True || b = True
-}

-- 5.
-- (&&) a b = if a == True then (if b == True then True else False) else False

-- 6.
-- (&&) a b = if a == True then b else False

-- 7.
mult_lamda :: Int -> Int -> Int -> Int
mult_lamda = \x -> (\y -> (\z -> x * y * z))
-- 8.
luhnDouble :: Int -> Int
luhnDouble x | x > 4 = (x * 2) - 9
             | otherwise = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn = \w -> (\x -> (\y -> (\z -> (luhnDouble w + x + luhnDouble y + z) `mod` 10 == 0)))


-- Chapter 5
-- 1.
sumSquares :: Int -> Int
sumSquares n = sum [x^2 | x <- [1..n]]

-- 2.
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- 3.
square :: Int -> [(Int, Int)]
square n = [(x,y) | x <- [0..n], y <- [0..n], x /= y]

-- 4.
replicate1 :: Int -> a -> [a]
replicate1 n a = [a | _ <- [1..n]]

-- 5.
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], z^2 == x^2 + y^2, x <= 10, y <= 10, z <= 10]

-- 6.
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
primes n = [x | x <- [1..n], factors x == [1,x]]
perfects n = [x | x <- [1..n], sum (factors (x)) - x == x]

-- 7.
-- [(x,y) | x <- [1,2], y <- [3,4]]
q7 :: [(Int,Int)]
q7 = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- 8.
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']
positions x xs = [i | (x',i) <- zip xs [0..], x == x']
positions1 x xs = find x [a | a <- zip xs [0..]]

-- 9. 
scalerProduct :: [Int] -> [Int] -> Int
scalerProduct xs ys = sum [x * y | (x,y) <- zip xs ys]

-- 10.
let2Int :: Char -> Int
let2Int c = ord c - ord 'a'

int2Let :: Int -> Char
int2Let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2Let ((let2Int c + n) `mod` 26)
          | isUpper c = int2Let (((let2Int c + n + 32) `mod` 26) - 32)
          | otherwise = c
-- Note: ord 'a' - ord 'z' = 32
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


































