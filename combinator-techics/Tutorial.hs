module Tutorial where

-- import Control.Applicative

data Person = Person { familyName :: Name 
                     , givenName :: Name
                     , age :: Age
                     }
              deriving Show
                       
type Name = String
type Age = Int

p1 = Person "Michael" "Smith" 45
p2 = Person "Michael" "Jordan" 49

(f <*> g) x = f x (g x)
infixl 4 <*>

www f g = f <*> g
xxx f g h = f <*> g <*> h
yyy f g h i = f <*> g <*> h <*> i
zzz f g h i j = f <*> g <*> h <*> i <*> j

xxx' f g h = f . g <*> h
yyy' f g h i = f . g <*> h <*> i
zzz' f g h i j = f . g <*> h <*> i <*> j

-- (.)   ::      (b -> c) -> (a -> b) -> a -> c
--               (b -> c1 -> c2 ...) -> (a -> b) -> a -> c1 -> c2 ...
-- (<*>) :: (a -> b -> c) -> (a -> b) -> a -> c
--          (a -> b -> c1 -> c2 ...) -> (a -> b) -> a -> c1 -> c2 ...
-- (.)と(<*>)の違いは一番最初の(a ->)を含むか含まないか.
-- xxxやyyyの型を見るとこの最初の(a ->)部分がないと便利そうに思えるはず.
-- つまり欲しいのは(.)だと解る.



type A1 = Int
type A2 = Int
type A3 = Int
type A4 = Int
type A5 = Int

arg1 :: (a -> b) -> c -> a -> b
arg1 = flip.(const.)

(<$>) :: (b -> c) -> (a -> b) -> a -> c
(<$>) = (<*>) . arg1

b f g x = f (g x)
s f g x = f x (g x)
t x y = y x
k x y = x
v x y z = z x y
r x y z = y z x
arg0 = b t (b k)
-- sid = b s (b t (b k))
-- sid f = b s (b t (b k)) f
-- sid f = s ((b t (b k)) f)
-- sid f g x = s ((b t (b k)) f) g x
-- sid f g x = ((b t (b k)) f) x (g x)
-- sid f g x = (b t (b k)) f x (g x)
-- sid f g x = b t (b k) f x (g x)
-- sid f g x = t ((b k) f) x (g x)
-- sid f g x = t (b k f) x (g x)
-- sid f g x = (b k f) (g x) x
-- sid f g x = b k f (g x) x
-- sid f g x = k (f (g x)) x
sid f g x = (f (g x))

s1 :: (a -> b -> c -> d -> e) -> (a -> b) -> a -> c -> d -> e
s1 f g x = f x (g x)
s2 :: (a -> c -> d -> e) -> (a -> c) -> a -> d -> e
s2 f g x = f x (g x)
s3 :: (a -> d -> e) -> (a -> d) -> a -> e
s3 f g x = f x (g x)
ss f g h = s2 (s1 f g) h
sss f g h i = s3 (s2 (s1 f g) h) i

--        val :: e
--          ^
--          |
-- s3       f''  g''
--          ^
--          |
--          | f'' :: a -> d -> e
--          | g'' :: a -> d
--          |
--    s2    f' g'
--          ^
--          |
--          | f' :: a -> c -> (d -> e)
--          | g' :: a -> c
--          |
--       s1 f g
--        f :: a -> b -> (c -> (d -> e))
--        g :: a -> b


hhh :: (A1 -> A2 -> A3) -> a1 -> b1
hhh = undefined

iii :: a1 -> (A1 -> A2 -> A3) -> b1
iii = undefined

jjj :: (A1 -> A2 -> A3) -> (A2 -> A1 -> A3)
jjj = undefined

(+++) x y z = x ++ y ++ z
(++++) x y z w = x ++ y ++ z ++ w
(+++++) x y z w w1 = x ++ y ++ z ++ w ++ w1
(++++++) x y z w w1 w2 = x ++ y ++ z ++ w ++ w1 ++ w2

-- name = ( (flip.(const.)) (+++) <*> familyName <*> const " " <*> givenName )
name = (+++) . familyName <*> const " " <*> givenName
age' = (+++) . const "(" <*> (show.age) <*> const ")"
prof = (++) . name <*> age'

----

-- myif p t f = p t f
-- myIf p t f = myif . ((toBool.) p) <*> t <*> f

myIf :: (a -> Bool' b) -> (a -> b) -> (a -> b) -> a -> b
myIf p t f = id . p <*> t <*> f

familyNameOfGivenName = myIf (toAge 48 .<. age) familyName givenName

toAge :: Int -> Person -> Age
toAge n _ = n

type Bool' a = a -> a -> a

apply p x y = p . x <*> y
personize = (((toBool.).).).apply

(.>.), (.<.), (.>=.), (.<=.), (.==.), (./=.) :: (Ord b) => (a -> b) -> (a -> b) -> (a -> Bool' c)
(.>.) = personize (>)
(.<.) = personize (<)
(.>=.) = personize (>=)
(.<=.) = personize (<=)
(.==.) = personize (==)
(./=.) = personize (/=)

personizel = (.const).personize
(>.), (<.), (>=.), (<=.), (==.), (/=.) :: (Ord b) => b -> (a -> b) -> (a -> Bool' c)
(>.) = personizel (>)
(<.) = personizel (<)
(>=.) = personizel (>=)
(<=.) = personizel (<=)
(==.) = personizel (==)
(/=.) = personizel (/=)

personizer = ((.const).).personize
(.>), (.<), (.>=), (.<=), (.==), (./=) :: (Ord b) => (a -> b) -> b -> (a -> Bool' c)
(.>) = personizer (>)
(.<) = personizer (<)
(.>=) = personizer (>=)
(.<=) = personizer (<=)
(.==) = personizer (==)
(./=) = personizer (/=)

-- (.++.) :: (a -> [b]) -> (a -> [b]) -> (a -> [b])
(.++.) :: Person's Name -> Person's Name -> Person's Name
-- (.++.) f g = (++) . f <*> g
-- (.++.) f g = ((++) . f) <*> g
-- (.++.) f g = ((.) (++) f) <*> g
(.++.) = (<*>).((++).)

-- (.<>.) op2 f g = op2 . f <*> g
-- (.<>.) op2 f g = (.) op2 f <*> g
-- (.<>.) op2 f g = (<*>) ((.) op2 f) g
-- (.<>.) op2 f = (<*>) ((.) op2 f)
-- (.<>.) op2 = (<*>). ((.) op2)
(.<>.) = ((<*>).) . (.)

-- (.<>.<>.) op3 f g h = op3 . f <*> g <*> h
-- (.<>.<>.) op3 f g h = (.) op3 f <*> g <*> h
-- (.<>.<>.) op3 f g h = (<*>) ((.) op3 f) g <*> h
-- (.<>.<>.) op3 f g h = (<*>) ((<*>) ((.) op3 f) g) h
-- (.<>.<>.) op3 f g = (<*>) ((<*>) ((.) op3 f) g)
-- (.<>.<>.) op3 f = ((<*>).) ((<*>) ((.) op3 f))
-- (.<>.<>.) op3 = (((<*>).).) ((<*>). ((.) op3))
-- (.<>.<>.) = (((<*>).).) . (((<*>).). (.))
(.<>.<>.) = (((<*>).).) . (.<>.)

(.<>.<>.<>.) = ((((<*>).).).) . (.<>.<>.)


-- s f g x = f x (g x)
-- b f' g' x' = f' (g' x)
-- b s f g x y = s (f g) x y= f g y (x y)

true = k
false = flip k
not' = v false true
(|||) = t true
(&&&) = r false

type Person's = (->) Person

toBool :: Bool -> a -> a -> a
toBool True = true
toBool False = false

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- b1 = b
-- ((.).(.)) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
-- b2 = b b b
-- ((.).(.).(.)) :: (b -> c) -> (a -> a1 -> a2 -> b) -> a -> a1 -> a2 -> c
--
-- b3 = b (b b b) b
-- b_n = b (b_n-1) b
-- となっている

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- b :: (t1 -> t) -> (t2 -> t1) -> t2 -> t
-- これがオリジナル
-- ((s.).) (.) :: (b -> t1 -> t) -> (t2 -> b) -> (t2 -> t1) -> t2 -> t
-- (((s.).).) (((s.).) (.))
--   :: (b -> t3 -> t1 -> t) -> (t2 -> b) -> (t2 -> t3) -> (t2 -> t1) -> t2 -> t
-- ((((s.).).).) ((((s.).).) (((s.).) (.)))
--   :: (b -> t4 -> t3 -> t1 -> t) -> (t2 -> b) -> (t2 -> t4) -> (t2 -> t3) -> (t2 -> t1) -> t2 -> t
--
-- 

-- name = ( (flip.(const.)) (+++) <*> familyName <*> const " " <*> givenName )
-- name = (((s.).).) (((s.).) b) (+++) familyName (const " ") givenName