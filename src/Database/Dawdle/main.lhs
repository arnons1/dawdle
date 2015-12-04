> {-# LANGUAGE ExplicitForAll,TupleSections,
>              NoMonomorphismRestriction,OverloadedStrings,
>              FlexibleContexts, RankNTypes #-}

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
> import Data.Time.Clock
> import Debug.Trace
> import System.Environment
> import qualified Data.Vector as V

> analyzeFile :: [[String]] -> Either String [CellType]
> analyzeFile mtrx = do
>   let cols = transpose mtrx
>   -- sanity on structure
>   unless (allTheSame (map length cols)) $ Left $ "Invalid CSV: Some rows have a different number of columns."
>   mapM workOnAColumn cols

> workOnAColumn :: [String] -> Either String CellType
> workOnAColumn [] = Left "Empty column, no cell types inferred"
> workOnAColumn cts = do
>   r' <- analyzeRow cts
>   foldM anlzr Unknown r'
>   where
>     anlzr :: CellType -> CellType -> Either String CellType
>     anlzr a b = do
>       let shouldBeNull = isNullable a || isNullable b
>       -- Check if previous format was the same format (for dates)
>       case (getDateFormatFromCT a,getDateFormatFromCT b) of
>         (Just d1, Just d2) | d1==d2 -> return $ applyNullIfNeeded shouldBeNull $ if b > a then b else a
>         _ -> return $ applyNullIfNeeded shouldBeNull $ maximum [b,a]

> analyzeRow :: [String] -> Either String [CellType]
> analyzeRow = mapM analyzeCell

> analyzeCell :: String -> Either String CellType
> analyzeCell s
>  | null s || isNullText s = Right $ Nullable Unknown
>  | b <- map toLower s
>  , b `elem` ["false","true"] = Right $ CTBool
>  | ((fFmt,_):_) <- catMaybes
>       [(fmt,) <$> ptm fmt s | fmt <- (iso8601DateFormat Nothing) : 
>                                      (if length s == 8 then ["%Y%m%d"] else [])] = 
>        return $ CTDate fFmt
>                           
>  | ((fFmt,_):_) <- catMaybes
>       [(fmt,) <$> ptm fmt s | fmt <- [iso8601DateFormat $ Just "%H:%M:%S"
>                                      ,"%Y-%m-%d %H:%M:%S.%Q"
>                                      ,"%Y-%m-%d %H:%M:%S"
>                                      ,rfc822DateFormat]] =
>        return $ CTDateTime fFmt
>  | otherwise = numTypeOrString s

> isNullText :: String -> Bool
> isNullText s = (map toLower s == "null") || (s == "\\N")

> ptm :: String -> String -> Maybe UTCTime
> ptm = parseTimeM True defaultTimeLocale

> numTypeOrString :: String -> Either String CellType
> numTypeOrString s
>   | isInt s = do
>       s' <- maybeToEither ("Can't read integer as Integer (??)") (readMaybe s :: Maybe Integer)
>       intType' s'
>   | (not . isInt) s && isCTNumber s = do
>      s' <- maybeToEither ("Can't read float type as Double (??)") (readMaybe s :: Maybe Double)
>      floatType s'
>   | otherwise = Right $ CTChar (length s)

> intType' :: Integer -> Either String CellType
> intType' x
>   | x < fromIntegral (maxBound :: Int8) && x > fromIntegral (minBound :: Int8) = Right CTInt8
>   | x < fromIntegral (maxBound :: Int16) && x > fromIntegral (minBound :: Int16) = Right CTInt16
>   | x < fromIntegral (maxBound :: Int32) && x > fromIntegral (minBound :: Int32) = Right CTInt32
>   | x < fromIntegral (maxBound :: Int64) && x > fromIntegral (minBound :: Int64) = Right CTInt64
>   | otherwise = Left "Error: Integer out of bounds"

> floatType :: Double -> Either String CellType
> floatType x = do
>   let x' = realToFrac x :: Float
>   if (show x') /= (show x)
>   then Right CTDouble
>   else Right CTFloat

> main :: IO ()
> main = do
>  a <- getArgs
>  opts <- getOpts a
>  let source = fromMaybe "(stdin)" $ (optInput . fst) opts
>  c <- case (optInput . fst) opts of
>         Just f -> LT.readFile f
>         Nothing -> do
>           LT.pack <$> getContents
>            
>  case parseCsv source c of
>           Left e -> do putStrLn "Error parsing input:"
>                        print e
>           Right r -> do
>             let rv = map (map V.fromList) r
>             putStrLn $ either error pretty $ analyzeFile rv


