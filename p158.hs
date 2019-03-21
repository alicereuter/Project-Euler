import Data.Char
import qualified Data.Set as S
nub' :: (Ord a) => [a] -> [a]
nub' = go S.empty
  where go _ [] = []
        go s (x:xs) | S.member x s = go s xs
                    | otherwise    = x : go (S.insert x s) xs

d x y = if ord(x) > ord(y) then 1 else 0 

--diff x  = scanl d (ord (head x)) $ map ord x



diff1 x  = sum $  tail $ zipWith d x (['a'] ++ init x)

fac x = product [1..x]

letters = [x:y:z:w:a:[] | x <- ['a'.. 'z'] ,y <- ['a'.. 'z'],z <- ['a' ..'z'], w <- ['a' .. 'z'] , a <- ['a' ..'z'] ]

--z = filter check  l

check = (\x -> if (diff1 x)== 1 then True else False)
  
--f x = if (nub' x) == x then True else False
--l = filter f letters

{-
2, 325
3, 10400
4, 164450
5, 1710280-}

choose x y = (fac x)/((fac y) *(fac (x-y)))

z = [(2,1),
    (3,4),
    (4,11),
    (5,26),
    (6,57),
    (7,120),
    (8,247),
    (9,502),
    (10,1013),
    (11,2036),
    (12,4083),
    (13,8178),
    (14,16369),
    (15,32753),
    (16,65519),
    (17,131054),
    (18,262125),
    (19,524268)]

f = map (\x -> (choose 26 (fst x)) * (snd x)) z

-- 409511334375
