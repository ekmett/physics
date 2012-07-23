module Physics.Matrix
  ( (!*!), (!*) , (*!)
  , M44, M43, m43_to_m44, translation
  ) where

import Data.Distributive
import Data.Functor
import Physics.V3
import Physics.V4
import Physics.Metric

infixl 7 !*!
-- | matrix product
(!*!) :: (Functor m, Metric r, Distributive n, Num a) => m (r a) -> r (n a) -> m (n a)
f !*! g = fmap (\r -> fmap (dot r) g') f
  where g' = distribute g

-- | matrix * column vector
infixl 7 *!
(!*) :: (Functor m, Metric r, Num a) => m (r a) -> r a -> m a
m !* v = dot v <$> m

infixl 7 !*
-- | row vector * matrix
(*!) :: (Metric r, Distributive n, Num a) => r a -> r (n a) -> n a
f *! g = dot f <$> distribute g

type M44 a = V4 (V4 a)
type M43 a = V4 (V3 a)

m43_to_m44 :: Num a => M43 a -> M44 a
m43_to_m44
  (V4 (V3 a b c)
      (V3 d e f)
      (V3 g h i)
      (V3 j k l)) =
  (V4 (V4 a b c 0)
      (V4 d e f 0)
      (V4 g h i 0)
      (V4 j k l 1))

-- extract the translation elements from the last row of a 4x3 or 4x4 matrix
translation :: (D3 v, Functor f) => (V3 a -> f (V3 a)) -> V4 (v a) -> f (V4 (v a))
translation = w . xyz

