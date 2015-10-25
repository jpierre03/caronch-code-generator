module Caronch.Lang.Data where

type Caronch = [Item]

data Item   = SimpleProcess Id
--            | LabeldProcess Id Label
            | Process Id Label Name
            | SimpleData Id
--            | LabeldData Id Label
            | Data Id Label Name
            | Link Id Id
            deriving (Show)

type Label = String
type Name = Nme String

newtype Id = Id String deriving (Eq, Ord, Show, Read)
