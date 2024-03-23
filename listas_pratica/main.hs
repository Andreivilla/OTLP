celsius_to_fahrenheit :: Fractional a => a -> a
celsius_to_fahrenheit a = a*1.8 +32

abono :: (Ord a, Num a, Num p) => a -> p
abono a 
    | a >= 1 && a <= 10 = 100
    | a > 10 && a <= 20 = 200
    | a > 20 && a <= 30 = 300
    | a > 30 && a <= 40 = 400
    | a > 40 = 500
    |otherwise = 0
  
is_par :: Int -> Bool
is_par a = if a `mod` 2 == 0 then True else False

bigger :: Ord p => p -> p -> p
bigger a b = if a > b then a else b

min_three :: Ord a => a -> a -> a -> a
min_three a b c
    | a < b && a < c = a
    | b < a && b < c = b
    |otherwise = c



main :: IO ()
main = do
  --print (celsius_to_fahrenheit 0)
  --print (abono 0)
  --print (is_par 5)
  --print(bigger 4 6)
  --print (min_three 8 2 3)