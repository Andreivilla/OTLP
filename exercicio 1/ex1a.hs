{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Show

ins :: Ord a => a -> Tree a -> Tree a
ins e Leaf = Node e Leaf Leaf
ins e n@(Node x l r)
  | e == x = n
  | e < x = Node x (ins e l) r
  | e > x = Node x l (ins e r)

--retirar elemento
--insere uma lista de elementos na arvore 
insList :: Ord a => [a] -> Tree a -> Tree a
insList xs tree = foldr ins tree xs

-- percorre a arvore em ordem
emOrdem :: Tree a -> [a]
emOrdem (Node a esq dir) = emOrdem esq ++ [a] ++ emOrdem dir
emOrdem Leaf = []
-- apaga o elemento da lista
delList :: Eq t => t -> [t] -> [t]
delList a [] = []
delList a (x:xs)
    | x == a = xs
    | otherwise = x : delList a xs
-- percorre apaga e remonta
delElementTree :: Ord a => a -> Tree a -> Tree a
delElementTree element tree = insList (delList  element (emOrdem tree)) tree

main :: IO ()
main = do
    let xs = [1,2,3,4,5]
    print xs
    let emptyTree = Leaf -- X is likely a typo, should be a Tree value
    let tree = insList xs emptyTree


    let element = 2
    print tree
    print(insList (delList  element (emOrdem tree)) tree)