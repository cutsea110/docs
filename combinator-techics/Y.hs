module Y where

y :: (a -> a) -> a
y = let f x = x (f x) in f

fib n | n < 2 = 1
      | otherwise = fib (n-1) + fib (n-2)

fibF f n | n < 2 = 1
         | otherwise = f (n-1) + f (n-2)

b f g x = f (g x)
s f g x = f x (g x)
