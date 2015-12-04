> {-# LANGUAGE FlexibleContexts, DeriveDataTypeable, RankNTypes, KindSignatures #-}
 
> module Database.Dawdle.Types
>        (SParser
>        ,CellType(..)
>        ,isNullable
>        ,applyNullIfNeeded
>        ,getDateFormatFromCT
>        ,removeNull
>        ,composeNull)
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

> removeNull :: CellType -> CellType
> removeNull (Nullable x) = x
> removeNull x = x

> composeNull :: CellType -> CellType -> CellType
> composeNull _ r@Nullable{} = r
> composeNull Nullable{} r = Nullable r
> composeNull _ r = r

> composeMaxTypesWithNulls :: CellType -> CellType -> CellType
> composeMaxTypesWithNulls mp nt =
>   composeNull mp (maximum $ map removeNull [mp,nt])

> isDateOrUnknown :: CellType -> Bool
> isDateOrUnknown CTDate{} = True
> isDateOrUnknown Unknown = True
> isDateOrUnknown _ = False

> isDateTimeOrUnknown :: CellType -> Bool
> isDateTimeOrUnknown CTDateTime{} = True
> isDateTimeOrUnknown Unknown = True
> isDateTimeOrUnknown _ = False
