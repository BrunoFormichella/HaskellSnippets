-- --------------------
--    Red Black Tree
-- --------------------

data Color = R | B deriving (Show)
data RBT a = E | T Color (RBT a) a (RBT a) deriving (Show)



memberRBT a E = False
memberRBT a (T _ l b r) | a==b = True
                        | a<b = memberRBT a l
                        | a>b = memberRBT a r


makeBlack E = E
makeBlack (T _ l x r) = T B l x r


balance B ( T R ( T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B ( T R a x (T R b y c)) z d  = T R (T B a x b) y (T B c z d)
balance B a x ( T R (T R b y c) z d)  = T R (T B a x b) y (T B c z d)
balance B a x ( T R b y (T R c z d))  = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r


insert x t = makeBlack (ins x t)
    where ins x E = T R E x E
          ins x (T c l y r) | x<y = balance c (ins x l) y r
                            | x>y = balance c l y (ins x r)
                            | otherwise = T c l y r


-- ---------------
--    LeftyHeap
-- ---------------

type Rank = Int
data Heap a = Empty | N Rank a (Heap a) (Heap a)


merge h1 Empty = h1
merge Empty h2 = h2
merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) =
         if x <= y then makeH x a1 (merge b1 h2)
         else           makeH y a2 (merge h1 b2)


rank Empty = 0
rank (N r _ _ _) = r


makeH x a b = if rank a >= rank b then N (rank b + 1) x a b
                                  else N (rank a + 1) x b a


insertH x h = merge (N 1 x Empty Empty) h


findMin (N _ x a b) = x


deleteMin (N _ x a b) = merge a b