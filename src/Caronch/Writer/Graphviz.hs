module Caronch.Writer.Graphviz where

import           Caronch.Lang.Data

writeGraphviz :: [Item] -> String
writeGraphviz items = begin_gv ++ body_gv ++ end_gv
    where body_gv = concatMap writeGraphviz' items
          begin_gv   = "digraph Caronch {" ++ eol ++ comments ++ eol
          end_gv     = "}" ++ eol
          comments   = unlines $
                        [ "//"
                        , "// This model is generated through CARONCH formalism."
                        , "// To create a picture from this model, you need Graphviz :"
                        , "// dot caronch.gv -Tpng -o caronch.png"
                        , "//"
                        , ""
                        ]
          eol = "\n"

writeGraphviz' :: Item -> String
writeGraphviz' item =
    case item of
        (SimpleData  id)    -> color_data ++ "data_"  ++ id ++ eol
        (SimpleProcess id)  -> color_process ++ "process_" ++ id ++ eol
        (Link id id')       -> id ++ " -> " ++ id' ++ eol
        _ -> "_ "
    where eol =";\n"
          color_data="node [shape=box,style=\"filled\",fillcolor=peachpuff]" ++ eol
          color_process="node [shape=ellipse,style=\"filled\",fillcolor=olivedrab1]" ++ eol

