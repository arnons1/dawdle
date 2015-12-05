
> {-# LANGUAGE ExplicitForAll,LambdaCase,
>              NoMonomorphismRestriction,OverloadedStrings,
>              FlexibleContexts, RankNTypes #-}

> module Database.Dawdle.PrettyPrint
> where
> import Database.Dawdle.Types
> import Text.PrettyPrint

> pretty :: String -> [String] -> [CellType] -> String
> pretty fn colns = render . ddl fn colns

> ddl :: String -> [String] -> [CellType] -> Doc
> ddl fn colns cts = text "create table" <+> text fn <+> body
>   where
>     body = parens $ nest 3 $ vcat $ punctuate comma clnms
>     clnms = zipWith (\a b -> text a <+> b) colns $ map (prettyTypes True) cts

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
>   CTChar i -> intp $ text "varchar" <> (parens (int i))
>   Unknown -> intp $ text "!!UNKNOWN!!"
>   where
>     intp tn = if isNullPass
>               then tn <+> text "not null"
>               else tn

