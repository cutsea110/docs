module B where

f :: a -> b
f = undefined
g :: b -> c
g = undefined
h :: a -> c
h = g . f
-- (.f) :: (b -> c) -> a -> c なので...
-- (.f) g
h' = (.f) g
-- つまり g . f は (.) g f でもいいが (g .) f でもいいし実は (. f) g でも同じ


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith' f xs ys = map (uncurry f) (zip xs ys)
-- zipWith' f xs = (map (uncurry f)) . (zip xs)
-- zipWith' f = (map (uncurry f) .) . zip
-- zipWith' f = (.zip) (map (uncurry f) .)
-- zipWith' f = (.zip) (map (uncurry f) .) -- 上記の法則より
-- zipWith' f = (.zip) ((.) (map (uncurry f)))
-- zipWith' f = (.zip) $ (.) $ map $ uncurry f
zipWith' = (.zip) . (.) . map . uncurry


-- zipWith' = (.zip).(.).map.uncurry
