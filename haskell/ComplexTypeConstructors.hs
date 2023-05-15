data Pair a b = MyPair a b deriving Show

instance Functor (Pair a) where
  fmap f (MyPair a b) = MyPair a (f b)

instance Applicative (Pair a) where
  pure x = MyPair undefined x
  MyPair a f <*> MyPair b x = MyPair a (f x)

instance Monad (Pair a) where
  MyPair a b >>= f = f b

sum10 :: Num x => x -> x
sum10 a = a + 10

