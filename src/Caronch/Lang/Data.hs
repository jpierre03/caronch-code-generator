module Caronch.Lang.Data where

type Caronch = [Item]

data Item   = Process Id Label Name
            | Data Id Label Name
            | Link Id Id
            deriving (Show)

type Id = String
type Label = String
type Name = String
