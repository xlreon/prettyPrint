module Test.Main where

import Main
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (mapWithIndex)
import Data.String.Utils (fromCharArray)
import Test.QuickCheck (Result, quickCheck, (<?>))
import Type.Data.Boolean (kind Boolean)
import Data.Map (Map(..), singleton)


checkInt :: Int -> Boolean
checkInt n = if printVal n == show n  then true else false

checkString :: String -> Boolean
checkString n = if printVal n == show n then true else false

checkArray :: Array String -> Boolean
checkArray arr = if printVal arr == (fromCharArray (mapWithIndex (\i n -> (showArray i n)) arr)) then true else false

checkMap :: forall k v. Show k => Pretty v => Map k v -> Boolean
checkMap mapObj@(Two left k v right) = if (printVal mapObj) == (showMapArray k v) then true else false
checkMap _ = false

main :: forall e. Eff (exception :: EXCEPTION, random :: RANDOM, console :: CONSOLE | e) Unit
main = do
  log "\nTest check for Print Val Integer\n"
  quickCheck checkInt
  log "\nTest check for Print Val String\n"
  quickCheck checkString
  log "\nTest check for Print Array\n"
  quickCheck checkArray

