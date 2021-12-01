{-# LANGUAGE RankNTypes #-}
module Almanac.Internal.Lens where

{- | 
Type synonyms and helpers to provide optics, without incurring a @Lens@
dependency.

See:

* [Relude's Lens](https://github.com/kowainik/relude/blob/3b2f4f85521c0558caafece937da44ecbbb54355/src/Relude/Extra/Lens.hs)
* [Write yourself a lens](https://vrom911.github.io/blog/write-yourself-a-lens)
* [How can I write lenses without depending on lens?](https://github.com/ekmett/lens/wiki/How-can-I-write-lenses-without-depending-on-lens%3F)
-}

-- | General lens: can change the type of the container.
-- from: https://hackage.haskell.org/package/lens-5.1/docs/Control-Lens-Lens.html#t:Lens
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | Monomorphic (simple) 'Lens': can't change the type when setting
type Lens' s a = Lens s s a a

-- | General Traversal
-- from: https://hackage.haskell.org/package/lens-5.1/docs/Control-Lens-Type.html#t:Traversal
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- | Monomorphic 'Traversal'
type Traversal' s a = Traversal s s a a

-- | Create a 'Lens'' from a getter and setter
simpleLens :: (s -> a) -> (s -> a -> s) -> Lens' s a
simpleLens getter setter f s = setter s <$> f (getter s)
