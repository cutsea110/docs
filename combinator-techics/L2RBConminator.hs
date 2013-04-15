module L2RBConminator where

zw :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zw f = (map (uncurry f).).zip
-- zw f = ((.) (map (uncurry f))).zip
-- zw f = (.zip) ((.) (map (uncurry f)))
-- zw f = (.zip) $ (.) (map (uncurry f))
-- zw f = (.zip) $ (.) ((map . uncurry) f)
-- zw f = (.zip) $ ((.).(map . uncurry)) f
-- zw = (.zip).(.).(map.uncurry)
zw = (.zip).(.).map.uncurry

f :: (a -> b -> c -> d)
f = undefined

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 = undefined

zw3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
-- zw3 f = ((map (uncurry3 f).).).zip3
-- zw3 f = (.zip3) ((map (uncurry3 f).).)
-- zw3 f = (.zip3) ((.) (map (uncurry3 f).))
-- zw3 f = (.zip3) $ (.) $ (.) (map (uncurry3 f))
-- zw3 f = (.zip3) $ (.) $ (.) $ (map.uncurry3) f
zw3 = (.zip3).(.).(.).map.uncurry3
