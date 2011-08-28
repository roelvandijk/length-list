{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, UnicodeSyntax #-}

module Data.LList
    ( LList
    , fromList
    , toList
    , prepend
    , (++)
    , head
    , last
    , tail
    , init
    , null
    , length
    , map
    , reverse
    , intersperse
    , intercalate
    , concat
    , take
    , drop
    , splitAt
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import qualified Data.List as L ( head, last, init, length
                                , reverse, intersperse
                                , foldr, foldl, foldr1, foldl1, map
                                , genericTake, genericDrop
                                )
import Control.Applicative ( Applicative, pure, (<*>)
                           , Alternative, empty, (<|>)
                           )
import Control.Monad       ( Monad, return, (>>=), (>>), fail, ap )
import Data.Bool           ( Bool(False, True), otherwise )
import Data.Eq             ( Eq, (==), (/=) )
import Data.Foldable       ( Foldable, foldr, foldl, foldr1, foldl1 )
import Data.Function       ( ($) )
import Data.Functor        ( Functor, fmap )
import Data.Monoid         ( Monoid, mempty, mappend )
import Data.Ord            ( Ord, compare, (<), min, max )
import Data.Typeable       ( Typeable )
import Prelude             ( Num, (+), (-)
                           , fromIntegral, error, seq )
import Text.Show           ( Show )

-- from base-unicode-symbols:
import Data.Eq.Unicode       ( (≡), (≢) )
import Data.Function.Unicode ( (∘) )
import Prelude.Unicode       ( ℤ, (⋅) )

-- from deepseq:
import Control.DeepSeq ( NFData, rnf )


--------------------------------------------------------------------------------
-- LList, a list with a known length
--------------------------------------------------------------------------------

data LList α = LList !ℤ [α]
               deriving (Show, Typeable)


--------------------------------------------------------------------------------
-- Basic list functions
--------------------------------------------------------------------------------

fromList ∷ [α] → LList α
fromList xs = LList (fromIntegral $ L.length xs) xs

toList ∷ LList α → [α]
toList (LList _ xs) = xs

prepend ∷ α → LList α → LList α
prepend x (LList n xs) = LList (n + 1) (x : xs)

(++) ∷ LList α → LList α → LList α
(++) = mappend

head ∷ LList α → α
head = L.head ∘ toList

last ∷ LList α → α
last = L.last ∘ toList

tail ∷ LList α → LList α
tail (LList n (_:xs)) = LList (n - 1) xs
tail _                = error "Data.LList.tail: empty list"

init ∷ LList α → LList α
init (LList n xs) = LList (n - 1) (L.init xs)

null ∷ LList α → Bool
null (LList n _) = n ≡ 0

length ∷ LList α → ℤ
length (LList n _) = n

map ∷ (α → β) → LList α → LList β
map f (LList n xs) = LList n (L.map f xs)

reverse ∷ LList α → LList α
reverse (LList n xs) = LList n (L.reverse xs)

intersperse ∷ α → LList α → LList α
intersperse e (LList n xs) = LList (if n < 2 then n else 2⋅n - 1)
                                   (L.intersperse e xs)

intercalate ∷ LList α → LList (LList α) → LList α
intercalate xs xss = concat (intersperse xs xss)

concat ∷ LList (LList α) → LList α
concat = foldr mappend mempty

take ∷ ℤ → LList α → LList α
take i (LList n xs) = LList (min i n) (L.genericTake i xs)

drop ∷ ℤ → LList α → LList α
drop i (LList n xs) = LList (max 0 (n-i)) (L.genericDrop i xs)

splitAt ∷ ℤ → LList α → (LList α, LList α)
splitAt n l = (take n l, drop n l)


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance (NFData α) ⇒ NFData (LList α) where
    rnf (LList _ xs) = rnf xs -- Note that the length is already evaluated
                              -- since it has a strictness flag.

instance Foldable LList where
    foldr  f z = L.foldr  f z ∘ toList
    foldl  f z = L.foldl  f z ∘ toList
    foldr1 f   = L.foldr1 f   ∘ toList
    foldl1 f   = L.foldl1 f   ∘ toList

instance (Eq α) ⇒ Eq (LList α) where
    (LList nx xs) == (LList ny ys)
        | nx ≢ ny  = False
        | otherwise = xs ≡ ys
    (LList nx xs) /= (LList ny ys)
        | nx ≢ ny  = True
        | otherwise = xs ≢ ys

instance (Ord α) ⇒ Ord (LList α) where
    compare (LList _ xs) (LList _ ys) = compare xs ys

instance Monoid (LList α) where
    mempty = fromList []
    mappend (LList nx xs) (LList ny ys) = LList (nx + ny) (xs `mappend` ys)

instance Functor LList where
    fmap = map

instance Applicative LList where
    pure = return
    (<*>) = ap

instance Alternative LList where
    empty = mempty
    (<|>) = mappend

instance Monad LList where
    m >>= k = foldr (mappend ∘ k)        mempty m
    m >>  k = foldr (mappend ∘ (\_ → k)) mempty m
    return = fromList ∘ (: [])
    fail _ = mempty
