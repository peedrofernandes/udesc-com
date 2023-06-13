import qualified Data.Map as Map
import Control.Monad.State

newtype MyMap = MyMap (Map.Map [Char] Integer) deriving Show
type MapState a = State MyMap a

addItem :: [Char] -> Integer -> MapState () 
addItem str int = do { (MyMap mapState) <- get
                     ; let updatedElement = Map.insert str int mapState
                     ; put $ MyMap updatedElement }

