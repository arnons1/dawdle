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


> type SParser a = forall s u (m :: * -> *). Stream s m Char => ParsecT s u m a
> data CellType = Nullable CellType | 
>   CTBool | CTInt8 | CTInt16 | CTInt32 | CTInt64 | CTChar Int | CTDate | CTDateTime | CTFloat | CTDouble | Unknown
>   deriving (Eq,Show,Ord,Data,Typeable)
> precedenceOrder =
>   [CTBool
>   ,CTInt8
>   ,CTInt16
>   ,CTInt32
>   ,CTInt64
>   ,CTFloat
>   ,CTDouble
>   ,CTDate
>   ,CTDateTime]
>   -- If all else fails, chars!

> analyzeCell :: String -> Either String CellType
> analyzeCell s = rm s

> rm :: String -> Either String CellType
> rm s
>  | null s = Right $ Nullable Unknown
>  | isInt s = do
>      s' <- maybeToEither ("Can't read integer as Integer (??)") (readMaybe s :: Maybe Integer)
>      intType s'
>  | (not . isInt) s && isCTNumber s = do
>      s' <- maybeToEither ("Can't read float type as Double (??)") (readMaybe s :: Maybe Double)
>      floatType s'
>  | otherwise = Right $ CTChar (length s)

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
>   

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
>           Right r -> print (map (map analyzeCell) r)

> sepChar :: Char
> sepChar = ','
> newLines :: String
> newLines = "\n\r"
> quoteChars :: String
> quoteChars = "'\""

> maybeToEither :: String -> Maybe a -> Either String a
> maybeToEither _ (Just x) = Right x
> maybeToEither s Nothing = Left s