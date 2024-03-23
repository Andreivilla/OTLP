{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import System.IO ()
--declare
--V = void Z = zero U - um
data Bin =  V | Z Bin | U Bin

data SigBin = Neg Bin | Pos Bin

--show data types
instance Show Bin where
    show V = ""
    show (Z b) = "0" ++ show b
    show (U b) = "1" ++ show b

instance Show SigBin where 
    show (Neg b) = "-" ++ show b
    show (Pos b) = "+" ++ show b

--operadores 
--conversão binario para inteiro
len :: Bin -> Int
len V = 0
len (Z b) = 1 + len b
len (U b) = 1 + len b

--Funções para converter binario para int
bin_to_int_no_sig :: (Num a1, Integral a2) => Bin -> a2 -> a1
bin_to_int_no_sig V _ = 0
bin_to_int_no_sig (Z b) len = bin_to_int_no_sig b (len - 1) 
bin_to_int_no_sig (U b) len = (2^len) + bin_to_int_no_sig b (len - 1)

bin_to_int :: Bin -> Integer
bin_to_int b = bin_to_int_no_sig b (len b - 1)

--bin_to_int_sinal :: (Integral a2, Num a1, Num (a2 -> a1)) => SigBin -> a2 -> a1
bin_to_int_sig :: SigBin -> Integer
bin_to_int_sig (Neg bin) = bin_to_int bin * (-1)
bin_to_int_sig (Pos bin) = bin_to_int bin


concat_bin :: Bin -> Bin -> Bin
concat_bin a V = a
concat_bin V b = b
concat_bin (Z a) b = Z (concat_bin a b)
concat_bin (U a) b = U (concat_bin a b)


--conversão de int para bin
integer_to_bin :: Integer -> Bin
integer_to_bin n
    | n `rem` 2 == 0 && n `div` 2 == 0 = Z V
    | n `rem` 2 == 0 = concat_bin (integer_to_bin (n `div` 2)) (Z V)
    | n `div` 2 == 0 = U V
    | otherwise = concat_bin (integer_to_bin (n `div` 2)) (U V)

integer_to_sig :: Integer -> SigBin
integer_to_sig n
    | n < 0 = Neg (integer_to_bin n)
    | otherwise = Pos (integer_to_bin n)

sig_to_integer :: SigBin -> Integer
sig_to_integer (Neg bin) = bin_to_int bin * (-1)
sig_to_integer (Pos bin) =bin_to_int bin



--(+) - Soma .
--(-) - Subtrai .
--(*) - Multiplica .
--negate - Negativa.
--abs - Retorna o valor absoluto.
--signum - Retorna o sinal de um número (-1, 0 ou 1).
--fromInteger - Converte um inteiro para qualquer tipo que seja membro de Num.

instance Num Bin where
    a + b = integer_to_bin (bin_to_int a + bin_to_int b)

    a - b = integer_to_bin (bin_to_int a - bin_to_int b)

    a * b = integer_to_bin (bin_to_int a * bin_to_int b)

    --não tem negate pq não tem sinal na classe com sinal vai ter
    
    abs b = b

    signum V = 0
    signum (Z b) = 0
    signum (U b) = 1

    fromInteger = integer_to_bin


instance Num SigBin where
    bin1 + bin2 = integer_to_sig (sig_to_integer bin1 + sig_to_integer bin2)

    bin1 - bin2 = integer_to_sig (sig_to_integer bin1 - sig_to_integer bin2)

    bin1 * bin2 = integer_to_sig (sig_to_integer bin1 * sig_to_integer bin2)

    fromInteger = integer_to_sig

    negate (Pos b) = Neg b
    negate (Neg b) = Pos b
    
    abs (Pos b) = Pos b
    abs (Neg b) = Pos b

    --signum :: SigBin -> SigBin
    signum (Pos V) = Pos (Z V)
    signum (Pos (Z V)) = Pos (Z V)
    signum (Neg (Z V)) = Pos (Z V)
    signum (Neg b) = Neg (U V)
    signum (Pos b) = Pos (U V)


main :: IO ()
main = do
  let binarioA = Pos (U (Z (Z (Z (U V)))))  
  let binarioB = Neg (U (U (U (Z (U V)))))  
  print (binarioA * binarioB)
