> {-# LANGUAGE FlexibleContexts, DeriveDataTypeable, RankNTypes, KindSignatures #-}
 
> module Database.Dawdle.Types
>        (SParser
>        ,CellType(..)
>        ,isNullable
>        ,applyNullIfNeeded
>        ,getDateFormatFromCT)
> where
> import Data.Data
> import Text.Parsec

> type SParser a = forall s u (m :: * -> *). Stream s m Char => ParsecT s u m a
> data CellType = Nullable CellType | Unknown | 
>   CTBool | CTInt8 | CTInt16 | CTInt32 | CTInt64 | CTFloat | CTDouble | CTDate String | CTDateTime String | CTChar Int
>   deriving (Eq,Show,Ord,Data,Typeable)

> isNullable :: CellType -> Bool
> isNullable (Nullable _) = True
> isNullable _ = False

> applyNullIfNeeded :: Bool -> CellType -> CellType
> applyNullIfNeeded True r@Nullable{} = r
> applyNullIfNeeded True r = Nullable r
> applyNullIfNeeded False r = r

> getDateFormatFromCT :: CellType -> Maybe String
> getDateFormatFromCT (CTDateTime s) = Just s
> getDateFormatFromCT (CTDate s) = Just s
> getDateFormatFromCT _ = Nothing
