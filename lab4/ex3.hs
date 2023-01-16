data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a) |
              Subtract (Expr a) (Expr a) |
              Multiply (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Subtract e1 e2) = eval e1 - eval e2
eval (Multiply e1 e2) = eval e1 * eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Subtract e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show' (Multiply e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = max (depthOfBT lt) (depthOfBT rt) + 1


-- flattenBT :: BinTree a -> [a] -- napisać trzy wersje: preorder, inorder, postorder
preflattenBT :: BinTree a -> [a]  
preflattenBT (EmptyBT) = []
preflattenBT (NodeBT v lt rt) = [v] ++ (preflattenBT lt) ++ (preflattenBT rt)

inflattenBT :: BinTree a -> [a]  
inflattenBT (EmptyBT) = []
inflattenBT (NodeBT v lt rt) = (inflattenBT lt) ++ [v] ++ (inflattenBT rt)

postflattenBT :: BinTree a -> [a]  
postflattenBT (EmptyBT) = []
postflattenBT (NodeBT v lt rt) = (postflattenBT lt) ++ (postflattenBT rt) ++ [v]
mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBT mapFun EmptyBT = EmptyBT
mapBT mapFun (NodeBT v lt rt) = NodeBT (mapFun v) (mapBT mapFun lt) (mapBT mapFun rt)

insert :: Ord a => a -> BinTree a -> BinTree a -- insert element into BinTree
insert val EmptyBT = NodeBT val EmptyBT EmptyBT
insert val (NodeBT v lt rt) = if val < v
                              then insert val lt
                              else insert val rt

--list2BST :: Ord a => [a] -> BinTree a -- list to Binary Search Tree (BST)
-- coś tam...

-- part from ex5
binTreeEqual :: Eq a => (BinTree a) -> (BinTree a) -> Bool
binTreeEqual EmptyBT EmptyBT = True
binTreeEqual EmptyBT (NodeBT _ _ _) = False
binTreeEqual (NodeBT v1 lt1 rt1) (NodeBT v2 lt2 rt2) = v1 == v2 && (binTreeEqual lt1 rt1) && (binTreeEqual lt2 rt2)  

instance Eq a => Eq (BinTree a) where
    (==) bt1 bt2 = binTreeEqual bt1 bt2 