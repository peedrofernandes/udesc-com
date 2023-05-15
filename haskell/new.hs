-- (<*>) :: Applicative f -> f (a -> b) -> f a -> f b

data Maybe2 a = Just2 a | Nothing2 deriving Show

instance Functor Maybe2 where
  fmap f (Just2 a) = Just2 (f a)
  fmap f (Nothing2) = Nothing2

instance Applicative Maybe2 where
  pure a = Just2 a
  -- Just2 f <*> (Just2 v) = Just2 (f v)
  Just2 f <*> v = fmap f v 

