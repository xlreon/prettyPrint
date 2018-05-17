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

instance printArray :: Pretty (Array String) where
  printVal :: Array String -> String
  printVal arr = fromCharArray (mapWithIndex (\i n -> showArray i n) arr)

instance printMapInt :: Pretty (Map String (Array Int)) where
  printVal :: (Map String (Array Int))-> String
  printVal (Two left k v right) = showMapArrayInt v k
  printVal _ = "more than 1 nodes"

instance printMapString :: Pretty (Map String (Array String)) where
  printVal :: (Map String (Array String)) -> String
  printVal (Two left k v right) = showMapArrayString v k
  printVal _ = "more than 1 node"

instance printArrayIntMap :: Pretty (Array (Map String (Array Int))) where 
  printVal :: (Array (Map String (Array Int))) -> String
  printVal arr = fromCharArray (map (\n -> printVal n) arr)

instance printArrayStringMap :: Pretty (Array (Map String (Array String))) where
  printVal :: (Array (Map String (Array String))) -> String
  printVal arr = fromCharArray (map (\n -> printVal n) arr) 

showArray :: Int -> String -> String
showArray index value = show index <> "\n" <> " " <> value <> "\n"

showMapArrayInt :: Array Int -> String -> String
showMapArrayInt value key = show key <> "\n" <> " " <> show value <> "\n"

showMapArrayString :: Array String -> String -> String
showMapArrayString value key = show key <> "\n" <> " " <> show value <> "\n"   


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "\nPretty Integer\n"
  log (printVal 1)
  log "\nPretty String\n"
  log (printVal "Hello")
  log "\nPretty Array\n"
  log (printVal ["hello","world"])
  log "\nPretty Map Array String\n"
  log (printVal [singleton "a" ["x","Y","z"],singleton "b" ["m","n"]])
  log "\nPretty Map Array Integer\n"
  log (printVal [singleton "a" [1,2,3],singleton "b" [8,9]])
