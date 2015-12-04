> {-# LANGUAGE ExplicitForAll,TupleSections,
>              NoMonomorphismRestriction,OverloadedStrings,
>              FlexibleContexts, RankNTypes #-}

> module Database.Dawdle.PrettyPrint
> where
> import Database.Dawdle.Types
> import Text.PrettyPrint

> pretty :: [CellType] -> String
> pretty = render . ddl

> ddl :: [CellType] -> Doc
> ddl cts = parens $ nest 3 $ vcat $ (map (text . show)) cts
