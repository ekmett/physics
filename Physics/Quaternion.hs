{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
module Physics.Quaternion
  ( Quaternion(..)
  , Complicated(..)
  , Hamiltonian(..)
  , slerp
  , asinq
  , acosq
  , atanq
  , asinhq
  , acoshq
  , atanhq
  , absi
  , pow
  , rotate
  ) where

import Control.Applicative
import Control.Lens
import Control.Lens.Rep
import Data.Complex (Complex((:+)))
import Data.Data
import Data.Distributive
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Physics.Epsilon
import Physics.Involutive
import Physics.Metric
import Physics.V3
import Physics.Vector
import Prelude hiding (any)

data Quaternion a = Quaternion a a a a deriving (Eq,Ord,Read,Show,Data,Typeable)

instance Functor Quaternion where
  fmap f (Quaternion e i j k) = Quaternion (f e) (f i) (f j) (f k)
  a <$ _ = Quaternion a a a a

instance Applicative Quaternion where
  pure a = Quaternion a a a a
  Quaternion f g h i <*> Quaternion a b c d = Quaternion (f a) (g b) (h c) (i d)

instance Monad Quaternion where
  return = pure
  (>>=) = bindRep

instance Representable Quaternion where
  rep f = Quaternion (f e) (f i) (f j) (f k)

instance Foldable Quaternion where
  foldMap f (Quaternion e i j k) = f e `mappend` f i `mappend` f j `mappend` f k
  foldr f z (Quaternion e i j k) = f e (f i (f j (f k z)))

instance Traversable Quaternion where
  traverse f (Quaternion e i j k) = Quaternion <$> f e <*> f i <*> f j <*> f k

instance RealFloat a => Num (Quaternion a) where
  {-# SPECIALIZE instance Num (Quaternion Float) #-}
  {-# SPECIALIZE instance Num (Quaternion Double) #-}
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  negate = fmap negate
  Quaternion a1 b1 c1 d1 * Quaternion a2 b2 c2 d2 = Quaternion
    (a1*a2 - b1*b2 - c1*c2 - d1*d2)
    (a1*b2 + b1*a2 + c1*d2 - d1*c2)
    (a1*c2 - b1*d2 + c1*a2 + d1*b2)
    (a1*d2 + b1*c2 - c1*b2 + d1*a2)
  fromInteger x = Quaternion (fromInteger x) 0 0 0
  abs z = Quaternion (norm z) 0 0 0
  signum q@(Quaternion e i j k)
    | m == 0.0 = q
    | not (isInfinite m || isNaN m) = q ^/ sqrt m
    | any isNaN q = qNaN
    | not (ii || ij || ik) = Quaternion 1 0 0 0
    | not (ie || ij || ik) = Quaternion 0 1 0 0
    | not (ie || ii || ik) = Quaternion 0 0 1 0
    | not (ie || ii || ij) = Quaternion 0 0 0 1
    | otherwise = qNaN
    where
      m = quadrance q
      ie = isInfinite e
      ii = isInfinite i
      ij = isInfinite j
      ik = isInfinite k

  -- abs    = error "Quaternion.abs: use norm"
  -- signum = error "Quaternion.signum: use signorm"

qNaN :: RealFloat a => Quaternion a
qNaN = Quaternion fNaN fNaN fNaN fNaN where fNaN = 0/0

-- {-# RULES "abs/norm" abs x = Quaternion (norm x) 0 0 0 #-}
-- {-# RULES "signum/signorm" signum = signorm #-}

-- this will attempt to rewrite calls to abs to use norm intead when it is available.

instance RealFloat a => Fractional (Quaternion a) where
  {-# SPECIALIZE instance Fractional (Quaternion Float) #-}
  {-# SPECIALIZE instance Fractional (Quaternion Double) #-}
  Quaternion q0 q1 q2 q3 / Quaternion r0 r1 r2 r3 = Quaternion (r0*q0+r1*q1+r2*q2+r3*q3)
                                                               (r0*q1-r1*q0-r2*q3+r3*q2)
                                                               (r0*q2+r1*q3-r2*q0-r3*q1)
                                                               (r0*q3-r1*q2+r2*q1-r3*q0)
                                                 ^/ (r0*r0 + r1*r1 + r2*r2 + r3*r3)
  recip q = q ^/ quadrance q
  fromRational x = Quaternion (fromRational x) 0 0 0

instance Metric Quaternion where
  Quaternion e i j k `dot` Quaternion e' i' j' k' = e*e' + i*i' + j*j' + k*k'

class Complicated t where
  e :: Functor f => (a -> f a) -> t a -> f (t a)
  i :: Functor f => (a -> f a) -> t a -> f (t a)

instance Complicated Complex where
  e f (a :+ b) = (:+ b) <$> f a
  i f (a :+ b) = (a :+) <$> f b

instance Complicated Quaternion where
  e f (Quaternion a b c d) = (\a' -> Quaternion a' b c d) <$> f a
  i f (Quaternion a b c d) = (\b' -> Quaternion a b' c d) <$> f b

class Complicated t => Hamiltonian t where
  j :: Functor f => (a -> f a) -> t a -> f (t a)
  k :: Functor f => (a -> f a) -> t a -> f (t a)
  ijk :: Functor f => (V3 a -> f (V3 a)) -> t a -> f (t a)

instance Hamiltonian Quaternion where
  j f (Quaternion a b c d) = (\c' -> Quaternion a b c' d) <$> f c
  k f (Quaternion a b c d) = Quaternion a b c <$> f d

  ijk f (Quaternion a b c d) = (\(V3 b' c' d') -> Quaternion a b' c' d') <$> f (V3 b c d)

instance Distributive Quaternion where
  distribute f = Quaternion (fmap (^.e) f) (fmap (^.i) f) (fmap (^.j) f) (fmap (^.k) f)

instance (Involutive a, Num a) => Involutive (Quaternion a) where
  conjugate (Quaternion e i j k) = Quaternion (conjugate e) (-i) (-j) (-k)


reimagine :: RealFloat a => a -> a -> Quaternion a -> Quaternion a
reimagine r s (Quaternion _ i j k)
  | isNaN s || isInfinite s = Quaternion r
    (if i /= 0 then i * s else 0)
    (if j /= 0 then j * s else 0)
    (if k /= 0 then k * s else 0)
  | otherwise = Quaternion r (i * s)  (j * s) (k * s)

-- | quadrance of the imaginary component
qi :: Num a => Quaternion a -> a
qi (Quaternion _ i j k) = i*i + j*j + k*k

absi :: Floating a => Quaternion a -> a
absi = sqrt . qi

pow :: RealFloat a => Quaternion a -> a -> Quaternion a
pow q t = exp (t *^ log q)

-- ehh..
instance RealFloat a => Floating (Quaternion a) where
  {-# SPECIALIZE instance Floating (Quaternion Float) #-}
  {-# SPECIALIZE instance Floating (Quaternion Double) #-}
  pi = Quaternion pi 0 0 0
  exp q@(Quaternion e i j k)
    | qiq == 0 = Quaternion (exp e) i j k
    | ai <- sqrt qiq, ee <- exp e = reimagine (ee * cos ai) (ee * (sin ai / ai)) q
    where qiq = qi q
  log q@(Quaternion e i j k)
    | qiq == 0 = if e >= 0 then Quaternion (log e) i j k else Quaternion (log (negate e)) pi j k -- mmm, pi
    | ai <- sqrt qiq, m <- sqrt (e*e + qiq) = reimagine (log m) (atan2 m e / ai) q
    where qiq = qi q
  x ** y = exp (y * log x)
  sqrt q@(Quaternion e i j k)
    | m   == 0 = q
    | qiq == 0 = if e > 0 then Quaternion (sqrt e) 0 0 0 else Quaternion 0 (sqrt (negate e)) 0 0
    | im <- sqrt (0.5*(m-e)) / sqrt qiq = Quaternion (0.5*(m+e)) (i*im) (j*im) (k*im)
    where qiq = qi q
          m = sqrt (e*e + qiq)
  cos q@(Quaternion e i j k)
    | qiq == 0 = Quaternion (cos e) i j k
    | ai <- sqrt qiq = reimagine (cos e * cosh ai) (- sin e * (sinh ai / ai)) q
    where qiq = qi q
  sin q@(Quaternion e i j k)
    | qiq == 0 = Quaternion (sin e) i j k
    | ai <- sqrt qiq = reimagine (sin e * cosh ai) (cos e * (sinh ai / ai)) q
    where qiq = qi q
  tan q@(Quaternion e i j k)
    | qiq == 0 = Quaternion (tan e) i j k
    | ai <- sqrt qiq, ce <- cos e, sai <- sinh ai, d <- ce*ce + sai*sai =
      reimagine (ce * sin e / d) (cosh ai * (sai / ai) / d) q
    where qiq = qi q
  sinh q@(Quaternion e i j k)
    | qiq == 0 = Quaternion (sinh e) i j k
    | ai <- sqrt qiq = reimagine (sinh e * cos ai) (cosh e * (sin ai / ai)) q
    where qiq = qi q
  cosh q@(Quaternion e i j k)
    | qiq == 0 = Quaternion (cosh e) i j k
    | ai <- sqrt qiq = reimagine (cosh e * cos ai) ((sinh e * sin ai) / ai) q
    where qiq = qi q
  tanh q@(Quaternion e i j k)
    | qiq == 0 = Quaternion (tanh e) i j k
    | ai <- sqrt qiq, se <- sinh e, cai <- cos ai, d <- se*se + cai*cai =
      reimagine ((cosh e * se) / d) ((cai * (sin ai / ai)) / d) q
    where qiq = qi q

  asin q = cut asin q
  acos q = cut acos q
  atan q = cut atan q

  asinh q = cut asinh q
  acosh q = cut acosh q
  atanh q = cut atanh q

cut :: RealFloat a => (Complex a -> Complex a) -> Quaternion a -> Quaternion a
cut f q@(Quaternion e _ j k)
  | qiq == 0 = Quaternion a b j k
  | otherwise = reimagine a (b / ai) q
  where qiq = qi q
        ai = sqrt qiq
        a :+ b = f (e :+ ai)

cutWith :: RealFloat a => Complex a -> Quaternion a -> Quaternion a
cutWith (r :+ im) q@(Quaternion e i j k)
  | e /= 0 || qiq == 0 || isNaN qiq || isInfinite qiq = error "bad cut"
  | s <- im / sqrt qiq = Quaternion r (i*s) (j*s) (k*s)
  where qiq = qi q

asinq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
asinq q@(Quaternion e _ _ _) u
  | qiq /= 0.0 || e >= -1 && e <= 1 = asin q
  | otherwise = cutWith (asin (e :+ sqrt qiq)) u
  where qiq = qi q

acosq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
acosq q@(Quaternion e _ _ _) u
  | qiq /= 0.0 || e >= -1 && e <= 1 = acos q
  | otherwise = cutWith (acos (e :+ sqrt qiq)) u
  where qiq = qi q

atanq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
atanq q@(Quaternion e _ _ _) u
  | e /= 0.0 || qiq >= -1 && qiq <= 1 = atan q
  | otherwise = cutWith (atan (e :+ sqrt qiq)) u
  where qiq = qi q

asinhq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
asinhq q@(Quaternion e _ _ _) u
  | e /= 0.0 || qiq >= -1 && qiq <= 1 = asinh q
  | otherwise = cutWith (asinh (e :+ sqrt qiq)) u
  where qiq = qi q

acoshq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
acoshq q@(Quaternion e _ _ _) u
  | qiq /= 0.0 || e >= 1 = asinh q
  | otherwise = cutWith (acosh (e :+ sqrt qiq)) u
  where qiq = qi q

atanhq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
atanhq q@(Quaternion e _ _ _) u
  | qiq /= 0.0 || e > -1 && e < 1 = atanh q
  | otherwise = cutWith (atanh (e :+ sqrt qiq)) u
  where qiq = qi q

slerp :: RealFloat a => Quaternion a -> Quaternion a -> a -> Quaternion a
slerp q p t
  | 1.0 - cosphi < 1e-8 = q
  | phi <- acos cosphi, r <- recip (sin phi)
  = (sin ((1-t)*phi)*r *^ q ^+^ f (sin (t*phi)*r) *^ p) ^/ sin phi
  where
   dqp = dot q p
   (cosphi, f) = if dqp < 0 then (-dqp, negate) else (dqp, id)
{-# SPECIALIZE slerp :: Quaternion Float -> Quaternion Float -> Float -> Quaternion Float #-}
{-# SPECIALIZE slerp :: Quaternion Double -> Quaternion Double -> Double -> Quaternion Double #-}

--slerp :: RealFloat a => Quaternion a -> Quaternion a -> a -> Quaternion a
--slerp q0 q1 = let q10 = q1 / q0 in \t -> pow q10 t * q0

rotate :: Num a => Quaternion a -> V3 a -> V3 a
rotate (Quaternion a' b c d) (V3 x y z) = V3
  (2*((t8+t10)*x+(t6- t4)*y+(t3+t7)*z)+x)
  (2*((t4+ t6)*y+(t5+t10)*y+(t9-t2)*z)+y)
  (2*((t7- t3)*z+(t2+ t9)*z+(t5+t8)*z)+z)
  where
    a = -a'
    t2 = a*b
    t3 = a*c
    t4 = a*d
    t5 = -b*b
    t6 = b*c
    t7 = b*d
    t8 = -c*c
    t9 = c*d
    t10 = -d*d
{-# SPECIALIZE rotate :: Quaternion Float -> V3 Float -> V3 Float #-}
{-# SPECIALIZE rotate :: Quaternion Double -> V3 Double -> V3 Double #-}

instance (RealFloat a, Epsilon a) => Epsilon (Quaternion a) where
  nearZero = nearZero . quadrance
