module Transform where

import           Line
import           Control.Applicative
import qualified Data.Set        as S
import qualified Data.List       as L
import qualified Data.Map.Strict as M

import Data.Semigroup as Sem

newtype Transform a = Transform { getVV :: Vect (Vect a) }

instance (Show a) => Show (Transform a) where
    show = unlines . map (unwords . map show . toList) . toList . getVV

instance (Num t) => Sem.Semigroup (Transform t) where
    (<>) = comp

instance (Num t) => Monoid (Transform t) where
    mempty  = ident
    mappend = comp

rotX :: (Floating a) => a -> Transform a
rotX t = Transform $
    Vect (Vect 1 0 0 0)
         (Vect 0 (cos th) (-sin th) 0)
         (Vect 0 (sin th) ( cos th) 0)
         (Vect 0 0 0 1)
    where th = t * (pi/180)

rotY :: (Floating a) => a -> Transform a
rotY t = Transform $
    Vect (Vect ( cos th) 0 (sin th) 0)
         (Vect 0 1 0 0)
         (Vect (-sin th) 0 (cos th) 0)
         (Vect 0 0 0 1)
    where th = t * (pi/180)

rotZ :: (Floating a) => a -> Transform a
rotZ t = Transform $
    Vect (Vect (cos th) (-sin th) 0 0)
         (Vect (sin th) ( cos th) 0 0)
         (Vect 0 0 1 0)
         (Vect 0 0 0 1)
    where th = t * (pi/180)

ident :: (Num a) => Transform a
ident = Transform $
    Vect (Vect 1 0 0 0)
         (Vect 0 1 0 0)
         (Vect 0 0 1 0)
         (Vect 0 0 0 1)

scale :: (Num a) => a -> a -> a -> Transform a
scale x y z = Transform $
    Vect (Vect x 0 0 0)
         (Vect 0 y 0 0)
         (Vect 0 0 z 0)
         (Vect 0 0 0 1)

trans :: (Num a) => a -> a -> a -> Transform a
trans x y z = Transform $
    Vect (Vect 1 0 0 x)
         (Vect 0 1 0 y)
         (Vect 0 0 1 z)
         (Vect 0 0 0 1)

hermite :: (Num a) => Transform a
hermite = Transform $
    Vect (Vect 2 (-2) 1 1)
         (Vect (-3) 3 (-2) (-1))
         (Vect 0 0 1 0)
         (Vect 1 0 0 0)

bezier :: (Num a) => Transform a
bezier = Transform $
    Vect (Vect (-1)   3  (-3) 1)
         (Vect   3  (-6)   3  0)
         (Vect (-3)   3    0  0)
         (Vect   1    0    0  0)

circle :: (Floating a, Enum a) => a -> a -> a -> a -> [Vect a]
circle cx cy cz r =
    L.zipWith4 Vect xs ys (repeat cz) (repeat 1)
        where xs = sampleParam 128 (\t -> r * cos (t * 2 * pi) + cx)
              ys = sampleParam 128 (\t -> r * sin (t * 2 * pi) + cy)

genHermFxns :: (Fractional a, Read a) =>
    [String] -> ((a -> a), (a -> a))
genHermFxns args =
    let [x0, y0, x1, y1, dx0, dy0, dx1, dy1] = map read args
        hermX = pmult hermite (Vect x0 x1 dx0 dx1)
        hermY = pmult hermite (Vect y0 y1 dy0 dy1)
            in ((dot hermX . cubify), (dot hermY . cubify))

genBezFxns :: (Fractional a, Read a) =>
    [String] -> ((a -> a), (a -> a))
genBezFxns args =
    let [x0, y0, x1, y1, x2, y2, x3, y3] = map read args
        bezX = pmult bezier (Vect x0 x1 x2 x3)
        bezY = pmult bezier (Vect y0 y1 y2 y3)
            in ((dot bezX . cubify), (dot bezY . cubify))

cubify :: (Num a) => a -> Vect a
cubify x = Vect (x*x*x) (x*x) x 1

sampleParam :: (Enum a, Fractional a) => a -> (a -> b) -> [b]
sampleParam tMax f = map f [0, (1 / tMax) .. 1]

-- I give up
transpose :: Transform a -> Transform a
transpose (Transform (Vect (Vect a b c d) (Vect e f g h)
    (Vect i j k l) (Vect m n o p))) =
            Transform $ Vect (Vect a e i m) (Vect b f j n)
                (Vect c g k o) (Vect d h l p)

drawEdges :: (RealFrac a) => Color -> [Vect a] -> Screen -> Screen
drawEdges c edges = mconcat . map (drawLine c . uncurry Line) $ (pairOff redges)
    where redges = map (fmap round) edges

comp :: (Num a) => Transform a -> Transform a -> Transform a
comp t = Transform . mmult (transpose t) . getVV

mmult :: (Num a, Functor f) => Transform a -> f (Vect a) -> f (Vect a)
mmult t = fmap (pmult t)

pmult :: (Num a) => Transform a -> Vect a -> Vect a
pmult t = liftA2 dot tt . pure
    where tt = getVV t

dot :: (Num a) => Vect a -> Vect a -> a
dot p = sum . liftA2 (*) p
