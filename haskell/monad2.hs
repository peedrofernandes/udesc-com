data Maybe2 a = Just2 a | Nothing2 deriving Show

-- fmap :: Functor f => (a -> b) -> f a -> f b
instance Functor Maybe2 where
  fmap f (Just2 v) = Just2 (f v)
  fmap f Nothing2 = Nothing2

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
instance Applicative Maybe2 where
  pure = Just2
  Just2 f <*> Just2 v = Just2 (f v)
  Just2 f <*> Nothing2 = Nothing2

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
instance Monad Maybe2 where
  Just2 v >>= f = f v
  Nothing2 >>= f = Nothing2

half :: Int -> Maybe2 Int
half x = if even x
  then Just2 (x `div` 2)
  else Nothing2
