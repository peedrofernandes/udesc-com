data Address = ConstructAddress {
  n :: Int,
  country :: String
} deriving Show

data Person = ConstructPerson {
  name :: String,
  age :: Int,
  address :: Address
} deriving Show


greet :: Person -> String
greet p = "Hi, my name is " ++ name p ++ "!"

p1 = ConstructPerson {
  name = "Pedro",
  age = 20,
  address = ConstructAddress {
    n = 388,
    country = "Brasil"
  }
}

data Tree = Branch {
  value :: Int,
  left :: Tree,
  right :: Tree 
} | Leaf deriving Show

insertNode :: Int -> Tree -> Tree
insertNode v Leaf = Branch {
  value = v,
  left = Leaf,
  right = Leaf
}
insertNode v (Branch value left right)
  | v < value = Branch { 
    value = value,
    left = insertNode v left,
    right = right 
  }
  | otherwise = Branch { 
    value = value,
    left = left,
    right = insertNode v right
  }

