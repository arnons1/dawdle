> {-# LANGUAGE FlexibleContexts,ExplicitForAll,TupleSections,
>              NoMonomorphismRestriction,OverloadedStrings, KindSignatures,
>              RankNTypes, DeriveDataTypeable #-}

> import Text.Parsec.Text ()
> import Text.Parsec hiding (label) -- many, optional, (<|>), string,
> --import Text.Parsec.Expr
> --import Text.Parsec.String
> --import Text.Parsec.Perm
> --import Text.Parsec.Pos
> --import Debug.Trace
> import qualified Data.Text as T
> import Data.Text (Text)
> import Data.Data
> import Text.Read (readMaybe)
> import Control.Monad
> import Data.Char
> import Data.Int
> import Data.List
> import Data.Time.Format
> import Data.Time.Clock
> import Text.Show.Pretty
> import Text.PrettyPrint hiding (char)

> type SParser a = forall s u (m :: * -> *). Stream s m Char => ParsecT s u m a
> data CellType = Nullable CellType | Unknown | 
>   CTBool | CTInt8 | CTInt16 | CTInt32 | CTInt64 | CTFloat | CTDouble | CTDate String | CTDateTime String | CTChar Int
>   deriving (Eq,Show,Ord,Data,Typeable)

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
>     anlzr a b =
>       let shouldBeNull = isNullable a || isNullable b
>       in return $ applyNullIfNeeded shouldBeNull $ if b > a then b else a

> isNullable :: CellType -> Bool
> isNullable (Nullable _) = True
> isNullable _ = False

> applyNullIfNeeded :: Bool -> CellType -> CellType
> applyNullIfNeeded True r@Nullable{} = r
> applyNullIfNeeded True r = Nullable r
> applyNullIfNeeded False r = r

> analyzeRow :: [String] -> Either String [CellType]
> analyzeRow = mapM analyzeCell

> analyzeCell :: String -> Either String CellType
> analyzeCell s
>  | null s = Right $ Nullable Unknown
>  | b <- map toLower s
>  , b `elem` ["false","true"] = Right $ CTBool
>  | Just f <- msum $ map getFstIfJust
>                   [(fmt, ptm fmt s) | fmt <- [iso8601DateFormat Nothing,"%Y%m%d"]] 
>                 = Right $ CTDate f
>  | Just f <- msum $ map getFstIfJust
>                   [(fmt, ptm fmt s) | fmt <- [iso8601DateFormat $ Just "%H:%M:%S"
>                                               ,"%Y-%m-%d %H:%M:%S.%Q"
>                                               ,"%Y-%m-%d %H:%M:%S"
>                                               ,rfc822DateFormat]]
>                 = Right $ CTDateTime f
>   
>  | isInt s = do
>      s' <- maybeToEither ("Can't read integer as Integer (??)") (readMaybe s :: Maybe Integer)
>      intType s'
>  | (not . isInt) s && isCTNumber s = do
>      s' <- maybeToEither ("Can't read float type as Double (??)") (readMaybe s :: Maybe Double)
>      floatType s'
>  | otherwise = Right $ CTChar (length s)

> ptm :: String -> String -> Maybe UTCTime
> ptm = parseTimeM True defaultTimeLocale

> getFstIfJust :: (a, Maybe b) -> Maybe a
> getFstIfJust (a,Just _) = Just a
> getFstIfJust _ = Nothing

> isCTNumber :: String -> Bool
> isCTNumber = all (\x -> isNumber x || x `elem` ['-','.'])

> isInt :: String -> Bool
> isInt = all (\x -> isNumber x || x =='-')

> intType :: Integer -> Either String CellType
> intType x
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


> csvFile :: SParser [[String]]
> csvFile = endBy line eol
> line :: SParser [String]
> line = sepBy cell (char sepChar)
> cell :: SParser String
> cell = quotedCell <|> many (noneOf (sepChar:newLines))

> quotedCell :: SParser String
> quotedCell = do
>        sChar <- (oneOf quoteChars)
>        content <- many (quotedChar sChar)
>        _ <- char sChar <?> "quote at end of cell"
>        return content

> quotedChar :: Char -> SParser Char
> quotedChar csep =
>        noneOf [csep]
>    <|> try (string (replicate 2 csep) >> return csep)

> eol :: SParser String
> eol =   try (string newLines)
>     <|> try (string (reverse newLines))
>     <|> string "\n"
>     <|> string "\r"
>     <?> "end of line"

> parseCSV :: Text -> Either ParseError [[String]]
> parseCSV input = parse csvFile "(unknown)" input

> main :: IO ()
> main = do
>  c <- getContents
>  case parse csvFile "(stdin)" (T.pack c) of
>           Left e -> do putStrLn "Error parsing input:"
>                        print e
>           Right r -> do
>             putStrLn $ either error (render . ddl) $ analyzeFile r

> sepChar :: Char
> sepChar = ','
> newLines :: String
> newLines = "\n\r"
> quoteChars :: String
> quoteChars = "'\""

> maybeToEither :: String -> Maybe a -> Either String a
> maybeToEither _ (Just x) = Right x
> maybeToEither s Nothing = Left s

> allTheSame :: (Eq a) => [a] -> Bool
> allTheSame xs = and $ map (== head xs) (tail xs)

> ddl :: [CellType] -> Doc
> ddl cts = parens $ nest 3 $ vcat $ (map (text . show)) cts
