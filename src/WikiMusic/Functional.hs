{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Functional
  ( uncurry2,
    curry2,
    curry3,
    uncurry3,
  )
where

import Relude

uncurry2 :: (a -> b -> c -> d) -> ((a, b), c) -> d
uncurry2 = uncurry . uncurry

curry2 :: (((a, b), c) -> d) -> a -> b -> c -> d
curry2 = curry . curry

uncurry3 :: (a -> b -> c -> d -> e) -> (((a, b), c), d) -> e
uncurry3 = uncurry . uncurry . uncurry

curry3 :: ((((a, b), c), d) -> e) -> a -> b -> c -> d -> e
curry3 = curry . curry . curry
