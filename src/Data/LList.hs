{-# LANGUAGE DeriveDataTypeable
           , NoImplicitPrelude
           , PackageImports
           , UnicodeSyntax
  #-}

module Data.LList
    ( LList
    , fromList
    , toList
    , cons
    , (++)
    , head
    , safeHead
    , last
    , safeLast
    , tail
    , safeTail
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

import qualified "base" Data.List as L
    ( head, last, init, length
    , reverse, intersperse
    , foldr, foldl, foldr1, foldl1, map
    , genericTake, genericDrop
    )
import "base" Control.Applicative ( Applicative, pure, (<*>)
                                  , Alternative, empty, (<|>)
                                  )
import "base" Control.Monad       ( Monad, return, (>>=), (>>), fail, ap )
import "base" Data.Bool           ( Bool(False, True), otherwise )
import "base" Data.Eq             ( Eq, (==), (/=) )
import "base" Data.Foldable       ( Foldable, foldr, foldl, foldr1, foldl1 )
import "base" Data.Function       ( ($) )
import "base" Data.Functor        ( Functor, fmap )
import "base" Data.Maybe          ( Maybe(Nothing, Just) )
import "base" Data.Monoid         ( Monoid, mempty, mappend )
import "base" Data.Ord            ( Ord, compare, (<), min, max )
import "base" Data.Typeable       ( Typeable )
import "base" Prelude             ( Num, (+), (-), fromIntegral, error )
import "base" Text.Show           ( Show )
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡), (≢) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Prelude.Unicode       ( ℤ, (⋅) )
import "deepseq" Control.DeepSeq ( NFData, rnf )


--------------------------------------------------------------------------------
-- LList, a list with a known length
--------------------------------------------------------------------------------

data LList α = LList !ℤ [α] deriving (Show, Typeable)


--------------------------------------------------------------------------------
-- Basic list functions
--------------------------------------------------------------------------------

fromList ∷ [α] → LList α
fromList xs = LList (fromIntegral $ L.length xs) xs
{-# INLINE fromList #-}

toList ∷ LList α → [α]
toList (LList _ xs) = xs
{-# INLINE toList #-}

cons ∷ α → LList α → LList α
cons x (LList n xs) = LList (n + 1) (x : xs)

(++) ∷ LList α → LList α → LList α
(++) = mappend
{-# INLINE (++) #-}

head ∷ LList α → α
head = L.head ∘ toList
{-# INLINE head #-}

safeHead ∷ LList α → Maybe α
safeHead l | null l    = Nothing
           | otherwise = Just (head l)

last ∷ LList α → α
last = L.last ∘ toList
{-# INLINE last #-}

safeLast ∷ LList α → Maybe α
safeLast l | null l    = Nothing
           | otherwise = Just (last l)

tail ∷ LList α → LList α
tail (LList n (_:xs)) = LList (n - 1) xs
tail _                = error "Data.LList.tail: empty list"

safeTail ∷ LList α → Maybe (LList α)
safeTail l | null l    = Nothing
           | otherwise = Just $ tail l

init ∷ LList α → LList α
init (LList n xs) = LList (n - 1) (L.init xs)

null ∷ LList α → Bool
null (LList n _) = n ≡ 0
{-# INLINE null #-}

length ∷ LList α → ℤ
length (LList n _) = n
{-# INLINE length #-}

map ∷ (α → β) → LList α → LList β
map f (LList n xs) = LList n (L.map f xs)
{-# INLINE map #-}

reverse ∷ LList α → LList α
reverse (LList n xs) = LList n (L.reverse xs)
{-# INLINE reverse #-}

intersperse ∷ α → LList α → LList α
intersperse e (LList n xs) = LList (if n < 2 then n else 2⋅n - 1)
                                   (L.intersperse e xs)

intercalate ∷ LList α → LList (LList α) → LList α
intercalate xs xss = concat (intersperse xs xss)
{-# INLINE intercalate #-}

concat ∷ LList (LList α) → LList α
concat = foldr mappend mempty
{-# INLINE concat #-}

take ∷ ℤ → LList α → LList α
take i (LList n xs) = LList (min i n) (L.genericTake i xs)

drop ∷ ℤ → LList α → LList α
drop i (LList n xs) = LList (max 0 (n-i)) (L.genericDrop i xs)

splitAt ∷ ℤ → LList α → (LList α, LList α)
splitAt n l = (take n l, drop n l)
{-# INLINE splitAt #-}

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
