
> {-# LANGUAGE ExplicitForAll,TupleSections,
>              NoMonomorphismRestriction,OverloadedStrings,
>              FlexibleContexts, RankNTypes #-}

> module Database.Dawdle.Dawdle
>        (analyzeFile)
> where
> import           Database.Dawdle.Options
> import           Database.Dawdle.Types
> import           Database.Dawdle.Utils

> import           Control.Monad
> import           Data.Char
> import           Data.Int
> import           Data.List
> import           Data.Time.Format
> import           Data.Time.Calendar
> import           Text.Read (readMaybe)

> analyzeFile :: Options -> [[String]] -> Either String [CellType]
> analyzeFile o mtrx = do
>   let cols = transpose mtrx
>   -- sanity on structure
>   when (optVerifyIntegrity o) $ unless (allTheSame (map length cols))
>     $ Left "Invalid CSV: Some rows have a different number of columns.\nPerhaps your separator is wrong?"
>   mapM workOnAColumn cols

> workOnAColumn :: [String] -> Either String CellType
> workOnAColumn [] = Left "Empty column, no cell types inferred"
> workOnAColumn cts = 
>   foldM analyzeCell Unknown cts

> analyzeCell :: CellType -> String -> Either String CellType
> analyzeCell mp s
>  | null s || isNullText s = Right $ composeMaxTypesWithNulls mp $ Nullable Unknown
>  | b <- map toLower s
>  , b `elem` ["false","true"]
>  , removeNull mp `elem` [CTBool,Unknown] =
>      Right $ composeMaxTypesWithNulls mp CTBool
>  | isDateOrUnknown $ removeNull mp
>  , Just fFmt <- msum
>       [ ptm fmt s | fmt <- iso8601DateFormat Nothing : 
>                            ["%Y%m%d" | length s == 8] ] = 
>      case getDateFormatFromCT mp of
>        Just oFmt |
>          oFmt /= fFmt -> return $ composeMaxTypesWithNulls mp $ CTChar (length s)
>        _ -> return $ composeMaxTypesWithNulls mp $ CTDate fFmt
>  | isDateTimeOrUnknown $ removeNull mp
>  , Just fFmt <- msum
>       [ ptm fmt s | fmt <- [iso8601DateFormat $ Just "%H:%M:%S"
>                                      ,"%Y-%m-%d %H:%M:%S%Q"
>                                      ,"%Y-%m-%d %H:%M:%S"
>                                      ,rfc822DateFormat]] =
>      case getDateFormatFromCT mp of
>        Just oFmt |
>          oFmt /= fFmt -> return $ composeMaxTypesWithNulls mp $ CTChar (length s)
>        _ -> return $ composeMaxTypesWithNulls mp $ CTDateTime fFmt
>  | isInt s
>  , removeNull mp < CTDateTime ""
>  , Right s' <- maybeToEither "Can't read integer as Integer (??)" (readMaybe s :: Maybe Integer) =
>       composeMaxTypesWithNulls mp <$> intType' s'
>  | (not . isInt) s && isCTNumber s
>  , removeNull mp < CTChar 1
>  , Right s' <- maybeToEither "Can't read float type as Double (??)" (readMaybe s :: Maybe Double) =
>      composeMaxTypesWithNulls mp <$> floatType s'
>   | otherwise = Right $ composeMaxTypesWithNulls mp $ CTChar (length s)
> {-# INLINE analyzeCell #-}

> isNullText :: String -> Bool
> isNullText s = (map toLower s == "null") || (s == "\\N")

> ptm :: String -> String -> Maybe String
> ptm tf ts = maybe Nothing (const $ Just tf) (parseTimeM True defaultTimeLocale tf ts :: Maybe Day)

> intType' :: Integer -> Either String CellType
> intType' x
>   | x < fromIntegral (maxBound :: Int8) && x > fromIntegral (minBound :: Int8) = Right CTInt8
>   | x < fromIntegral (maxBound :: Int16) && x > fromIntegral (minBound :: Int16) = Right CTInt16
>   | x < fromIntegral (maxBound :: Int32) && x > fromIntegral (minBound :: Int32) = Right CTInt32
>   | x < fromIntegral (maxBound :: Int64) && x > fromIntegral (minBound :: Int64) = Right CTInt64
>   | otherwise = Left "Error: Integer out of bounds"

> floatType :: Double -> Either String CellType
> floatType x = 
>   let x' = realToFrac x :: Float
>   in if show x' /= show x
>      then Right CTDouble
>      else Right CTFloat
