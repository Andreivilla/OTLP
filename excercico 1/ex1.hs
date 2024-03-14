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

delIten :: Ord a => a -> Tree a -> Tree a
delIten _ Leaf = Leaf
delIten e (Node x l r)
    | e < x     = Node x (delIten e l) r
    | e > x     = Node x l (delIten e r)
    | otherwise = case (l, r) of
                    (Leaf, Leaf) -> Leaf
                    (Leaf, _)    -> r
                    (_, Leaf)    -> l
                    _            -> let (m, r') = minRem r
                                    in Node m l r'
  where
    minRem (Node x Leaf r) = (x, r)
    minRem (Node x l r)    = let (m, l') = minRem l
                              in (m, Node x l' r)


main :: IO ()
main = do
    let xs = [1,2,3,4,5]
    print xs
    let emptyTree = Leaf -- X is likely a typo, should be a Tree value
    let tree = insList xs emptyTree

    let tree_remov = delIten 3 tree

    print tree_remov