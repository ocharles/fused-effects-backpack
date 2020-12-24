{-# language FlexibleContexts #-}
{-# options -ddump-simpl -ddump-stg -dsuppress-all #-}

-- | Here we write our main business logic. In effect system code, we would
-- usually some type of constrained polymorphism to specify our base monad,
-- allowing the user to choose alternative instantiations. In the world of
-- Backpack, we instead import just the signature of the monad. This means we
-- can still use alternative instantiations - simply provide a different module
-- that matches the signature.

module BusinessLogic where

import BusinessLogic.Monad ( M )
import Control.Algebra ( Has )
import Control.Effect.Empty ( Empty, guard )

businessCode :: Has Empty sig M => Bool -> M Int
businessCode b = do
  guard b
  return 42
