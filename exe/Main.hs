module Main where

import BusinessLogic
import Control.Monad.Trans.Maybe ( runMaybeT )
import qualified BusinessLogic.Monad

main :: IO ()
main = print =<< BusinessLogic.Monad.runEmpty (businessCode True)
