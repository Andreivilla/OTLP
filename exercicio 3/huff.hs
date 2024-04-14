{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Data.List ( sort )
import Data.Word ( Word8, Word32 )
import qualified Data.Binary.Put as P
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I

data Huffman = Folha Int Char | No Int Huffman Huffman


n_freq :: Char -> String -> Int--frequencia de um caracter
n_freq x xs = sum [1 | c <- xs, c == x]

--frequencia de um caracter
del_duo :: String -> String
del_duo [] = []
del_duo (x:xs) = x : filter (/= x) (del_duo xs)

simb_freq :: String -> [Huffman]
simb_freq str = map (\c -> Folha (1 + n_freq c str) c) (del_duo str)
--

--Sobrecarregando os operadores do tipo Huffman para usar no sort
instance Eq Huffman where
    (Folha v1 _) == (Folha v2 _) = v1 == v2
    (Folha v1 _) == (No v2 _ _) = v1 == v2
    (No v1 _ _) == (Folha v2 _) = v1 == v2
    (No v1 _ _) == (No v2 _ _) = v1 == v2

instance Ord Huffman where
    compare (Folha v1 _) (Folha v2 _) = compare v1 v2
    compare (Folha v1 _) (No v2 _ _) = compare v1 v2
    compare (No v1 _ _) (Folha v2 _) = compare v1 v2
    compare (No v1 _ _) (No v2 _ _) = compare v1 v2
--
-- construindo arvore de huff
insert_no :: Huffman -> [Huffman] -> [Huffman]
insert_no no [] = [no]
insert_no no@(No v1 _ _) (x@(Folha v2 _) : xs) = do
    if v2 > v1 then no : x : xs else x : insert_no no xs
insert_no no@(No v1 _ _) (x@(No v2 _ _) : xs) = do
    if v2 > v1 then no : x : xs else x : insert_no no xs
insert_no no@(Folha v1 _) (x@(Folha v2 _) : xs) =
    if v2 > v1 then no : x : xs else x : insert_no no xs
insert_no no@(Folha v1 _) (x@(No v2 _ _) : xs) =
    if v2 > v1 then no : x : xs else x : insert_no no xs

build_huff_tree :: [Huffman] -> Huffman
build_huff_tree [] = error "lista vazia"
build_huff_tree [x] = x
build_huff_tree (f1@(Folha a b) : f2@(Folha c d) : xs) = build_huff_tree $ insert_no (No (a + c) f1 f2)  xs
build_huff_tree (f1@(Folha a b) : n1@(No v1 l1 r1) : xs) = build_huff_tree $ insert_no (No (a + v1) f1 n1) xs
build_huff_tree (n1@(No v1 l1 r1) : f1@(Folha a b) : xs) = build_huff_tree $ insert_no (No (a + v1) n1 f1) xs
build_huff_tree (n1@(No v1 l1 r1) : n2@(No v2 l2 r2) : xs) = build_huff_tree $ insert_no (No (v1 + v2) n1 n2) xs

add :: Char -> [(Char, String)] -> [(Char, String)]
add v [] = []
add v ((c, s) : xs) = (c, v : s) : add v xs

codHuffman :: Huffman -> [(Char, String)]
codHuffman (Folha _ c) = [(c, "")]
codHuffman (No _ l r) = add '0' (codHuffman l) ++ add '1' (codHuffman r)

get_cod :: Char -> [(Char, String)] -> String
get_cod _ [] = ""  -- Tratar caso da lista vazia
get_cod e ((c, s) : xs)
    | e == c = s
    | otherwise = get_cod e xs

--codifica palavra
encode :: String -> [(Char, String)] -> String
encode [] _ = []
encode (x:xs) mapeamento = get_cod x mapeamento ++ encode xs mapeamento

--decodifica palavra
decode :: String -> Huffman -> String
decode [] _ = []
decode _ (Folha v c) = show c
decode (x:xs) huff@(No v l r) = do
    if x == '0' then do
        let res = decode_a xs l
        let letra = fst res
        let resto = snd res
        letra : decode resto huff
    else do
        let res = decode_a xs r
        let letra = fst res
        let resto = snd res
        letra : decode resto huff
    where
        decode_a :: String -> Huffman -> (Char, String)
        decode_a xs (Folha v c) = (c, xs)
        decode_a (x:xs) (No v l r)
            | x == '0' = decode_a xs l
            | x == '1' = decode_a xs r
        decode_a _ _ = error "Nó invalido"

--numero de caracteres unicos no texto
n_char :: [Huffman] -> Int
n_char = foldr (\ x -> (+) 1) 0

--numero de caracteres montados apartir da arvore
len_char :: String -> Int
len_char = foldr (\ x -> (+) 1) 0

--função para adicionar valores ao vetor de huff
put :: [Huffman] -> String -> P.PutM ()
put freq codificado = do
    put_n_len (n_char freq) (len_char codificado)
    put_tuplas freq
    put_bin8 codificado
    where
        put_n_len :: Int -> Int -> P.PutM ()
        put_n_len a b = do
            P.putWord8 (toEnum a)
            P.putWord32be (toEnum b)
            P.flush
        put_tuplas :: [Huffman] -> P.PutM () --trata entradqa de tuplas
        put_tuplas [] = P.flush
        put_tuplas ((Folha v c) : xs) = do
            P.putWord8 (I.c2w c)
            P.putWord32be (toEnum v)
            put_tuplas xs
        put_tuplas (_: xs) = put_tuplas xs
        
        put_bin8 :: [Char] -> P.PutM ()--valores em binario de 8 em 8
        put_bin8 "" = P.flush
        put_bin8 xs = do
            let x = toInt 0 (reverse (take 8 xs))
            P.putWord8 (toEnum x)
            put_bin8 (drop 8 xs)
            where 
                toInt _ [] = 0
                toInt a (x:xs) = char_to_digit x * 2 ^ a + toInt (a+1) xs
                    where
                        char_to_digit x = if x == '0' then 0 else 1

--recuperar tuplas e frequnecias
get_freq :: (Eq t, Num t) => t -> G.Get [(Word8, Word32)]
get_freq qtd = do
    if qtd == 0 then return []
    else do
        k <- get_reg
        ks <- get_freq (qtd-1)
        return (k:ks)
        where
            get_reg :: G.Get (Word8, Word32)
            get_reg = do
                c <- G.getWord8
                f <- G.getWord32be
                return (c,f)

--Função para pegar todos os inteiros referentes ao binario do texto
get_int_bins :: G.Get [Word8]
get_int_bins = do
    empty <- G.isEmpty
    if empty then return []
    else do
        k <- get_bin
        ks <- get_int_bins
        return (k:ks)
    where
        --auxiliar da get_int_bins pega um bin
        get_bin :: G.Get Word8
        get_bin = do
            G.getWord8

--retorna todos os binarios
get_all_bins :: G.Get ((Word8, Word32), [(Word8, Word32)], [Word8])
get_all_bins = do
    vet@(n,t) <- get_first_tupla
    freq <- get_freq n
    binario <- get_int_bins
    return (vet, freq, binario)
    where
        --primeira tupla e a quantidade de caracteres no binario do texto codificado
        get_first_tupla :: G.Get (Word8, Word32)
        get_first_tupla = do
            c <- G.getWord8
            f <- G.getWord32be
            return (c,f)

--Função para construir um vetor [Huffman] a partir de tuplas (Word8, Word32)
tuplas_to_Huff :: Integral a => [(Word8, a)] -> [Huffman]
tuplas_to_Huff [] = []
tuplas_to_Huff ((c, v):xs) = do
    Folha (fromIntegral v) (I.w2c c) : tuplas_to_Huff xs

--vetor de words para string
bint8_to_string :: [Word8] -> [Char]
bint8_to_string [] = []
bint8_to_string (x:xs) = do
    I.w2c x : bint8_to_string xs

--Função para pegar um char e passar para inteiro
char_to_int :: Enum a => [a] -> [Int]
char_to_int [] = []
char_to_int (x:xs) = do
    fromEnum x : char_to_int xs

--Função para preencher uma string com 0 a esquerda até ela ter tamanho 8
zero_string :: String -> String
zero_string v = do
    if length v == 8 then v
    else zero_string ('0' : v)

--Função para preencher uma string com 0 a esquerda até ela ter tamanho k
complete_string :: Int -> String -> String
complete_string 0 v = v
complete_string k xs = complete_string (k-1) ('0' : xs)

vet_int_to_bin :: Integral a => Int -> [a] -> [Char]--Função para passar vetor de inteiros para uma string em binario
vet_int_to_bin _ [] = []
vet_int_to_bin tam (x:xs) = do
    --Se o x for o ultimo numero do vetor
    if null xs then do
        --Passo o numero para binario
        let k = int_to_bin x
        --Pego o tamanho do numero
        let tamK = length k
        --Se o tamanho do numero for menor do k, então preencha a string com 0 a esquerda até ter tamanho k
        if tamK < tam then complete_string (tam - tamK) k
        else k --Caso contrario, apenas retorne a string
    --Se não for o ultimo numero, então transforme para binario, preencha com 0 a esquerda até ter tamanho 8 e va para o proximo numero
    else zero_string (int_to_bin x) ++ vet_int_to_bin (tam - 8) xs
    --Definição da função para montar o binario de um inteiro
    where
        int_to_bin 0 = ""
        int_to_bin a = do
            int_to_bin (a `div` 2) ++ to_digit a
            where
                to_digit a = if even a then "0" else "1"

delete_extension :: String -> String
delete_extension [] = [] -- Se a string estiver vazia, retorna vazio
delete_extension name =
    if '.' `elem` name -- Verifica se há um ponto na string
        then takeWhile (/= '.') name -- Se houver, pega a parte do nome até o ponto
        else name

compress :: FilePath -> IO ()
compress arq = do
    texto <- readFile arq --le o conteudo do arquivo
    let freq = simb_freq texto --frequencia de caracteres
    let huffTree = build_huff_tree (sort freq) --arvore de huff
    let charMap = codHuffman huffTree --mapeia os caracteres para binario
    let codificado = encode texto charMap --codifica os dados do arquivo
    let bs = P.runPut (put freq codificado) -- cria a estrutura de armazenamento
    L.writeFile ( delete_extension arq ++ ".huff") bs-- por fim cria o arquivo de saida
    return ()

decompress :: FilePath -> IO ()
decompress arq = do
    bs <- L.readFile arq -- abre arquivo
    let ((c, v), freq, binarios) = G.runGet get_all_bins bs --pega dados do arquivo
    let sortedList = sort $ tuplas_to_Huff freq -- ordena lista sort
    let huffTree = build_huff_tree sortedList --constroi arvore
    let listaBinarioString = bint8_to_string binarios-- confere binario prar string
    let listaBinarioInt = char_to_int listaBinarioString -- passa os dados para inteiros
    let textoParaDecode = vet_int_to_bin (fromIntegral v) listaBinarioInt -- cria o codigo huff
    let res = decode textoParaDecode huffTree -- decodifica o texto
    writeFile (delete_extension arq ++ ".txt") res --arquivo de saida
    return ()

--use compress para comprimir o arquivo
--use decompress para decompor
main :: IO()
main = do
    --compress "loren.txt"
    decompress "loren.huff"