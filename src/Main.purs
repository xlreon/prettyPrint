module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (mapWithIndex)
import Data.Map (Map(..), singleton)
import Data.String.Utils (fromCharArray)

class Pretty a where
  printVal :: a -> String

instance printInt :: Pretty Int where
  printVal :: Int -> String
  printVal num = show num

instance printString :: Pretty String where
  printVal :: String -> String
  printVal val = show val  

instance printArray :: Show a => Pretty (Array a) where
  printVal :: Array a -> String
  printVal arr = fromCharArray (mapWithIndex (\i n -> (showArray i n)) arr)

instance printMap :: (Show k, Pretty v) => Pretty (Map k v) where
  printVal :: (Map k v)-> String
  printVal (Two left k v right) = showMapArray k v
  printVal _ = "more than 1 nodes"

showArray :: forall a b. Show a => Int -> a -> String
showArray index value = show index <> "\n" <> " " <> show value <> "\n"

showMapArray :: forall a b. Show a => a -> Pretty b => b -> String
showMapArray key value = show key <> "\n" <> " " <> (printVal value) <> "\n"


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "\nPretty Integer\n"
  log (printVal 1)
  log "\nPretty String\n"
  log (printVal "Hello")
  log "\nPretty Array\n"
  log (printVal ["hello","world"])
  log "\nPretty Map Array String\n"
  log (printVal (singleton "a" ["x","Y","z"]))
  log "\nPretty Map Array Integer\n"
  log (printVal (singleton "a" [1,2,3]))
