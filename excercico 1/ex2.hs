data RBTree a = Nil | Node Color a (RBTree a) (RBTree a) deriving(Show)
data Color = Red | Black deriving (Show, Eq)

member :: (Ord a) => a -> RBTree a -> Bool
member _ Nil = False
member x (Node _ y left right)
    | x < y = member x left
    | x > y = member x right
    | otherwise = True

insList :: Ord a => [a] -> RBTree a -> RBTree a
insList xs tree = foldr insert tree xs

blacken :: RBTree a -> RBTree a
blacken Nil = Nil
blacken (Node _ value left right) = Node Black value left right

insert :: (Ord a) => a -> RBTree a -> RBTree a
insert x root = blacken $ insert' root
  where insert' Nil = Node Red x Nil Nil
        insert' root@(Node color y left right)
            | x < y = balance color y (insert' left) right
            | x > y = balance color y left (insert' right)
            | otherwise = root

balance :: Color -> a -> RBTree a -> RBTree a -> RBTree a
balance Black z (Node Red x a (Node Red y b c)) d = Node Red y (Node Black x a b) (Node Black z c d)
balance Black z (Node Red y (Node Red x a b) c) d = Node Red y (Node Black x a b) (Node Black z c d)
balance Black x a (Node Red y b (Node Red z c d)) = Node Red y (Node Black x a b) (Node Black z c d)
balance Black x a (Node Red z (Node Red y b c) d) = Node Red y (Node Black x a b) (Node Black z c d)
balance color value left right = Node color value left right

main :: IO ()
main = do
    let tree = insert 14 (insert 10 (insert 17 Nil))  -- Insert some values
    putStrLn "Tree after initial insertions:"
    print tree

    let list = [5, 9, 16, 98]
    let newTree = insList list tree

    putStrLn "\nTree after inserting 5:"
    print newTree

