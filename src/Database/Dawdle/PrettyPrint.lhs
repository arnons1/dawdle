> {-# LANGUAGE ExplicitForAll,LambdaCase,
>              NoMonomorphismRestriction,OverloadedStrings,
>              FlexibleContexts, RankNTypes #-}

> module Database.Dawdle.PrettyPrint
> where
> import Database.Dawdle.Types
> import Text.PrettyPrint

> pretty :: String -> [CellType] -> String
> pretty fn = render . (ddl fn)

> ddl :: String -> [CellType] -> Doc
> ddl fn cts = text "create table" <+> text fn <+> body
>   where
>     body = parens $ nest 3 $ vcat $ (punctuate comma (map (prettyTypes True) cts))

> prettyTypes :: Bool -> CellType -> Doc
> prettyTypes isNullPass = \case
>   Nullable c -> prettyTypes False c <+> text "null"
>   CTBool -> intp $ text "bool"
>   CTInt8 -> intp $ text "tinyint"
>   CTInt16 -> intp $ text "smallint"
>   CTInt32 -> intp $ text "int"
>   CTInt64 -> intp $ text "bigint"
>   CTFloat -> intp $ text "real"
>   CTDouble -> intp $ text "float"
>   CTDate _ -> intp $ text "date"
>   CTDateTime _ -> intp $ text "datetime"
>   CTChar i -> intp $ text "varchar" <> (parens $ int i)
>   Unknown -> intp $ text "!!UNKNOWN!!"
>   where
>     intp tn = if isNullPass
>               then tn <+> text "not null"
>               else tn

