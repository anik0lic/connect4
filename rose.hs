module Rose (
    Rose(..),
    size,
    height,
    leavesCount,
    leaves,
    elemsOnDepth,
    foldRose,
    generateRose,
    greaterThen20
) where

data Rose a = Node a [Rose a] deriving (Show, Eq)

-- a)
size :: Rose a -> Int
size (Node _ children) = 1 + sum (map size children)

height :: Rose a -> Int
height (Node _ children) = 1 + maximum (0 : map height children)

-- b)
leavesCount :: Rose a -> Int
leavesCount (Node _ []) = 1
leavesCount (Node _ children) = sum (map leavesCount children)

leaves :: Rose a -> [a]
leaves (Node val []) = [val]
leaves (Node _ children) = concatMap leaves children

-- c)
elemsOnDepth :: Int -> Rose a -> [a]
elemsOnDepth 0 (Node val _) = [val]
elemsOnDepth n (Node _ children) = concatMap (elemsOnDepth (n-1)) children

-- d)
instance Functor Rose where
    fmap f (Node val children) = Node (f val) (map (fmap f) children)

-- e) foldRose (+) 0 (Node 1 [Node 2 [], Node 3 [Node 4 []]])
foldRose :: (a -> b -> b) -> b -> Rose a -> b
foldRose f acc (Node val children) = f val (foldl (foldRose f) acc children)

-- f) generateRose 2 (\x->[x+1,x+2,x+3]) 2
generateRose :: Int -> (a -> [a]) -> a -> Rose a
generateRose 0 _ root = Node root []
generateRose n f root = Node root (map (generateRose (n-1) f) (f root))

greaterThen20 :: (Ord a, Num a) => Rose a -> [a]
greaterThen20 = foldRose (\x acc -> if x > 20 then x : acc else acc) []