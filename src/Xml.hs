module Xml (XmlComponent(..), Xml, toXml) where

import Data.List
import Data.Map (Map)
import Data.Map qualified as M

data XmlComponent = XmlComponent String (Map String String) [XmlComponent] | Text String



showParams :: Map String String -> String
showParams = intercalate " " . map (\(x, y) -> x ++ "=" ++ show y) . M.toList

instance Show XmlComponent where
  show (XmlComponent name params []) = "<" ++ name ++ " " ++ showParams params ++ "/>"
  show (XmlComponent name params children) = "<" ++ name ++ " " ++ showParams params ++ ">" ++ concat (map show children) ++ "</" ++ name ++ ">"
  show (Text s) = s

class Xml a where
  toXml :: a -> XmlComponent
