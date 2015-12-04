> {-# LANGUAGE ExplicitForAll,TupleSections,
>              NoMonomorphismRestriction,OverloadedStrings,
>              FlexibleContexts, RankNTypes, ViewPatterns #-}

> import Database.Dawdle.Types
> import Database.Dawdle.PrettyPrint
> import Database.Dawdle.Parser
> import Database.Dawdle.Utils
> import Database.Dawdle.Options
> 
> import qualified Data.Text.Lazy.IO as LT
> import qualified Data.Text.Lazy as LT
> import Text.Parsec.Text ()
> 
> --import qualified Data.Text as T
> import Text.Read (readMaybe)
> import Control.Monad
> import Data.Maybe
> import Data.Char
> import Data.Int
> import Data.List
> import Data.Time.Format
> import Data.Time.Calendar
> import Debug.Trace
> import System.Environment
> import System.FilePath

> analyzeFile :: [[String]] -> Either String [CellType]
> analyzeFile mtrx = do
>   let cols = transpose mtrx
>   -- sanity on structure
>   unless (allTheSame (map length cols)) $ Left $ "Invalid CSV: Some rows have a different number of columns.\nPerhaps your separator is wrong?"
>   mapM workOnAColumn cols

> workOnAColumn :: [String] -> Either String CellType
> workOnAColumn [] = Left "Empty column, no cell types inferred"
> workOnAColumn cts = do
>   foldM analyzeCell Unknown cts

> analyzeCell :: CellType -> String -> Either String CellType
> analyzeCell mp s
>  | null s || isNullText s = Right $ composeMaxTypesWithNulls mp $ Nullable Unknown
>  | b <- map toLower s
>  , b `elem` ["false","true"]
>  , (removeNull mp) `elem` [CTBool,Unknown] =
>      Right $ composeMaxTypesWithNulls mp CTBool
>  | (isDateOrUnknown $ removeNull mp)
>  , Just fFmt <- msum
>       [ ptm fmt s | fmt <- (iso8601DateFormat Nothing) : 
>                            (if length s == 8 then ["%Y%m%d"] else [])] = 
>      case getDateFormatFromCT mp of
>        Just oFmt |
>          oFmt /= fFmt -> return $ composeMaxTypesWithNulls mp $ CTChar (length s)
>        _ -> return $ composeMaxTypesWithNulls mp $ CTDate fFmt
>  | (isDateTimeOrUnknown $ removeNull mp)
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
>  , (removeNull mp) < CTDateTime "" = do
>       s' <- maybeToEither ("Can't read integer as Integer (??)") (readMaybe s :: Maybe Integer)
>       composeMaxTypesWithNulls mp <$> intType' s'
>  | (not . isInt) s && isCTNumber s
>  , (removeNull mp) < CTChar 1 = do
>      s' <- maybeToEither ("Can't read float type as Double (??)") (readMaybe s :: Maybe Double)
>      composeMaxTypesWithNulls mp <$> floatType s'
>   | otherwise = Right $ composeMaxTypesWithNulls mp $ CTChar (length s)


> isNullText :: String -> Bool
> isNullText s = (map toLower s == "null") || (s == "\\N")

> ptm :: String -> String -> Maybe String
> ptm tf ts = maybe Nothing (const $ Just tf) ((parseTimeM True defaultTimeLocale tf ts) :: Maybe Day)


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
>   in if (show x') /= (show x)
>      then Right CTDouble
>      else Right CTFloat

> main :: IO ()
> main = do
>  a <- getArgs
>  opts <- getOpts a
>  let source = fromMaybe "stdin" $ (optInput . fst) opts
>      sepChar = (optSepChar . fst) opts
>  c <- case (optInput . fst) opts of
>         Just f -> LT.readFile f
>         Nothing -> do
>           LT.pack <$> getContents
>  case parseCsv sepChar source c of
>    Left e -> do
>      putStrLn "Error parsing input:"
>      print e
>    Right r -> do
>      putStrLn $ either error (pretty $ genTbName source) $ analyzeFile r
>  where
>    genTbName = takeBaseName


