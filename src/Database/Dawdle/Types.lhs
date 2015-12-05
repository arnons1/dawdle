
> {-# LANGUAGE FlexibleContexts, DeriveDataTypeable, RankNTypes, KindSignatures #-}
 
> module Database.Dawdle.Types
>        (SParser
>        ,CellType(..)
>        ,isNullable
>        ,addNullable
>        ,getDateFormatFromCT
>        ,removeNullable
>        ,composeNull
>        ,composeMaxTypesWithNulls
>        ,isDateOrUnknown
>        ,isDateTimeOrUnknown)
> where
> import Data.Data
> import Text.Parsec

> type SParser a = forall s u (m :: * -> *). Stream s m Char => ParsecT s u m a
> -- ^ Used to prettify the type signatures

> -- | The CellType contains the types available for inferrence.
> --   The order matters, as it represents the order in which the
> --   algorithm will try to infer.
> data CellType =
>                 Nullable CellType -- ^ Nullable cell (Recursive definition)
>               | Unknown -- ^ Initial type is unknown
>               | CTBool
>               | CTInt8
>               | CTInt16
>               | CTInt32
>               | CTInt64
>               | CTFloat
>               | CTDouble
>               | CTDate String -- ^ The string represents the format
>               | CTDateTime String -- ^ The string represents the format
>               | CTChar Int -- ^ String with size
>   deriving (Eq,Show,Ord,Data,Typeable)

> -- | Is this cell nullable?
> isNullable :: CellType -> Bool
> isNullable Nullable{} = True
> isNullable _ = False

> -- | Add nullability to a cell
> addNullable :: CellType -> CellType
> addNullable r@Nullable{} = r
> addNullable r = Nullable r

> -- | Remove nullability from a cell
> removeNullable :: CellType -> CellType
> removeNullable (Nullable x) = x
> removeNullable x = x

> -- | Compose second cell with first cell's nullability
> composeNull :: CellType -> CellType -> CellType
> composeNull _ r@Nullable{} = r
> composeNull Nullable{} r = Nullable r
> composeNull _ r = r

> -- | Select the maximum type between first and second cell, and apply
> --   nullability if needed (according to first cell)
> composeMaxTypesWithNulls :: CellType -> CellType -> CellType
> composeMaxTypesWithNulls mp nt =
>   composeNull mp (maximum $ map removeNullable [mp,nt])

> -- | Pull out format from date or datetime cells
> getDateFormatFromCT :: CellType -> Maybe String
> getDateFormatFromCT (CTDateTime s) = Just s
> getDateFormatFromCT (CTDate s) = Just s
> getDateFormatFromCT _ = Nothing

> -- | Is this cell a Date or Unknown?
> isDateOrUnknown :: CellType -> Bool
> isDateOrUnknown CTDate{} = True
> isDateOrUnknown Unknown = True
> isDateOrUnknown _ = False

> -- | Is this cell a DateTime or Unknown?
> isDateTimeOrUnknown :: CellType -> Bool
> isDateTimeOrUnknown CTDateTime{} = True
> isDateTimeOrUnknown Unknown = True
> isDateTimeOrUnknown _ = False
