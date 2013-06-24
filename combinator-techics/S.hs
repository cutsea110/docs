module S where

import Control.Applicative ((<$>), (<*>))

s :: (t2 -> t1 -> t) -> (t2 -> t1) -> t2 -> t
s f g x = f x (g x) -- s = (<*>)


s2 :: (t2 -> t3 -> t1 -> t) -> (t2 -> t3 -> t1) -> t2 -> t3 -> t
-- s2 f g x y = f x y (g x y)
-- s2 f g x y = (f x y) (g x y)
-- s2 f g x y = ((f x) y) ((g x) y)
-- s2 f g x y = (f' y) (g' y) -- f' = f x , g' = g x
-- s2 f g x y = (<*>) f' g' y
-- s2 f g x = (<*>) f' g'
-- s2 f g x = (<*>) (f x) (g x) -- (1)
-- s2 f g x = ((<*>) (f x)) (g x)
-- s2 f g x = ( ((<*>) . f) x ) (g x)
-- s2 f g x = (<*>) ((<*>) . f) g x -- (2)
-- s2 f g = (<*>) ((<*>) . f) g
-- s2 f g = ((<*>) . f) <*> g
-- s2 f g = (<*>) . f <*> g
s2 f g = (<*>) <$> f <*> g
