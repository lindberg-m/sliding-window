module Window where

import Control.Monad.State

updateState :: Monad m => (a -> s -> s) -> (s -> b) -> a -> StateT s m b
updateState f g x = state (\s -> (g s, f x s))


ordIf :: Ordering -> a -> a -> a -> a
ordIf LT x _ _ = x
ordIf EQ _ x _ = x
ordIf GT _ _ x = x   

s = [1..29]
f = (:) . (*) 2

