import Control.Monad.Trans.State
import Data.Functor.Identity
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Gen

computation:: StateT (Int, Int) Identity Int
computation = do
  modify (\tup -> case tup of
           (l, r) -> (l + r, 0))
  return 10


  
-- Functor rules:
-- fmap:: (a -> b) -> f a -> f b
-- Functor for either

myfmap:: (l -> l2) -> Either l r -> Either l2 r
myfmap f (Left l) = Left (f l)
myfmap f (Right r) = Right r

myfmap2:: (r -> r2) -> Either l r -> Either l r2
myfmap2 f (Left l) = Left l
myfmap2 f (Right r) = Right (f r)

myfmap3:: (a -> b) -> (b -> c) -> (a -> c)
myfmap3 f g = g . f

myfmap4:: (a -> b) -> (c -> a) -> (c -> b)
myfmap4 f g = f . g
