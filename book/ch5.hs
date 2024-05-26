import Data.Monoid
import Data.Semigroup hiding (Max, Min)

data TipoProduto = Escritorio | Informatica | Livro | Filme | Total

data Produto
  = Produto
      { valor :: Double,
        tp :: TipoProduto
      }
  | Nada

instance Semigroup Produto where
  (<>) :: Produto -> Produto -> Produto
  (<>) (Produto v1 _) (Produto v2 _) = Produto (v1 + v2) Total

instance Monoid Produto where
  mempty :: Produto
  mempty = Produto 0 Total

totalGeral :: [Produto] -> Double
totalGeral = valor . mconcat

--------------------------------------------------
newtype Min = Min Int deriving (Ord, Eq, Show)

instance Semigroup Min where
  (<>) :: Min -> Min -> Min
  (<>) (Min a) (Min b) = Min (min a b)

instance Monoid Min where
  mempty :: Min
  mempty = Min maxBound

minAll :: [Min] -> Min
minAll = mconcat

--------------------------------------------------

data Paridade = Par | Impar

class ParImpar a where
  decide :: a -> Paridade

instance ParImpar Int where
  decide :: Int -> Paridade
  decide i
    | even i = Par
    | odd i = Impar

instance ParImpar [a] where
  decide :: [a] -> Paridade
  decide a
    | even $ length a = Par
    | odd $ length a = Impar

instance ParImpar Bool where
  decide :: Bool -> Paridade
  decide False = Par
  decide True = Impar

--------------------------------------------------

newtype Max = Max Int deriving (Ord, Eq, Show)

instance Semigroup Max where
  (<>) :: Max -> Max -> Max
  (<>) (Max a) (Max b) = Max (max a b)

instance Monoid Max where
  mempty :: Max
  mempty = Max minBound

maxAll :: [Max] -> Max
maxAll = mconcat

--------------------------------------------------

data Tree a = Branch a (Tree a) (Tree a) | Leaf a | Empty deriving (Show)

treeMap :: (a -> a) -> Tree a -> Tree a
treeMap f (Branch b t1 t2) = Branch (f b) (treeMap f t1) (treeMap f t2)
treeMap f (Leaf l) = Leaf (f l)
treeMap _ Empty = Empty

-- treeMap (5 +) (Branch 60 (Branch 40 (Leaf 30) (Leaf 50)) (Branch 80 (Leaf 70) Empty))

--------------------------------------------------
