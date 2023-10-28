module Solution
    ( unique
    , pythagoreanTriples
    , primitivePythagoreanTriples
    , perfectNumbers
    , cantorPairs
    , minimalDistance
    ) where


unique :: Eq a => [a] -> Bool
unique [] = True
unique l = 
    let length :: [a] -> Int
        length [] = 0
        length (x:xs) = 1 + length xs
        len = length l
        pairs = [(l !! a, l !! b) | a <- [0..len-2], b <- [a+1..len-1]]
        unique' :: Eq a => [(a, a)] -> Bool
        unique' [] = True
        unique' (x:xs) = if fst x == snd x
            then False
            else unique' xs
    in unique' pairs


pythagoreanTriples :: Integral a => [(a, a, a)]
pythagoreanTriples = [(x, y, z) | z <- [2..], y <- [2..z-1], x <- [2..y-1], x ^ 2 + y ^ 2 == z ^ 2]


primitivePythagoreanTriples :: Integral a => [(a, a, a)]
primitivePythagoreanTriples = 
    let greatestCommonDivisor :: Integral a => a -> a -> a
        greatestCommonDivisor x 0 = x
        greatestCommonDivisor 0 y = y
        greatestCommonDivisor x y = if x > y
            then greatestCommonDivisor (x - y) y
            else greatestCommonDivisor x (y - x)
    in [(x, y, z) | z <- [2..], y <- [2..z-1], x <- [2..y-1], x ^ 2 + y ^ 2 == z ^ 2, greatestCommonDivisor x y == 1]


perfectNumbers :: Integral a => [a]
perfectNumbers =
    let greatestCommonDivisor :: Integral a => a -> a -> a
        greatestCommonDivisor x 0 = x
        greatestCommonDivisor 0 y = y
        greatestCommonDivisor x y = if x > y
            then greatestCommonDivisor (x - y) y
            else greatestCommonDivisor x (y - x)
        sumOfDivisors :: Integral a => a -> a -> a -> a
        sumOfDivisors number check accum
            | fromIntegral(check) > sqrt(fromIntegral(number)) = accum
            | greatestCommonDivisor number check == check = sumOfDivisors number (check + 1) (accum + check + number `div` check)
            | otherwise = sumOfDivisors number (check + 1) accum
    in [n | n <- [2..], sumOfDivisors n 1 0 == n * 2]
--Ехехе, я думал, что не зайдет, но оно посчитало 4-ое число примерно за 7 секунд
--Не до конца уверен, что это тот путь, который подразумевался, но оно работает


cantorPairs :: Integral a => [(a, a)]
cantorPairs = 
    let nextPair :: Integral a => (a, a) -> (a, a)
        nextPair (x, y) = if x == 0
            then (y + 1, 0)
            else (x - 1, y + 1)
        cantorPairIndex :: Integral a => a -> (a, a)
        cantorPairIndex 0 = (0, 0)
        cantorPairIndex i = nextPair(cantorPairIndex (i - 1))
    in [cantorPairIndex i | i <- [0,1..]]


minimalDistance :: RealFloat a => [(a, a)] -> a
minimalDistance [] = 1 / 0
minimalDistance [_] = 1 / 0
minimalDistance l =
    let length :: [a] -> Int
        length [] = 0
        length (x:xs) = 1 + length xs
        distance :: RealFloat a => (a, a) -> (a, a) -> a
        distance x y = sqrt((fst x - fst y) ^ 2 + (snd x - snd y) ^ 2)
    in minimum [distance (l !! x) (l !! y) | x <- [0,1..length l - 2], y <- [x + 1..length l - 1]]
