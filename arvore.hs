import System.Win32 (COORD(x))
data ArvBin a = Nil | No a (ArvBin a) (ArvBin a) deriving (Show, Eq)

-- Função para buscar um elemento na árvore
busca :: Ord a => a -> ArvBin a -> Bool
busca x Nil = False
busca x (No y esq dir)
    | x == y = True
    | x < y = busca x esq
    | x > y = busca x dir

-- Função para percorrer a árvore em ordem
emOrdem :: ArvBin a -> [a]
emOrdem Nil = []
emOrdem (No x esq dir) = emOrdem esq ++ [x] ++ emOrdem dir


--ordenar lista
removerPrimeiro :: Eq a => [a] -> a -> [a]
removerPrimeiro (x:xs) n = if x == n then xs else x:removerPrimeiro xs n

menorElemento :: Ord a => [a] -> a
menorElemento [x] = x
menorElemento (x : xs) = if x < menorElemento xs then x else menorElemento xs


ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar xs = menorElemento xs :ordenar (removerPrimeiro xs (menorElemento xs))


-- Exemplo de uso da função
main = do
    let arvoreExemplo = No 10 (No 5 (Nil) (No 7 Nil Nil)) (No 15 (No 12 Nil Nil) (No 20 Nil Nil))
    let lista = emOrdem arvoreExemplo
    print (ordenar lista)