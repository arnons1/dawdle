
> {-# LANGUAGE ExplicitForAll,TupleSections,
>              NoMonomorphismRestriction,OverloadedStrings,
>              FlexibleContexts, RankNTypes #-}

> module Database.Dawdle.Parser
>        (parseCsv)
> where
> import Database.Dawdle.Types
> import Data.Text.Lazy (Text)
> import Text.Parsec

> -- | Parses a CSV file and returns a 2D list of parsed strings
> parseCsv :: Char -- ^ Separator character for CSV
>          -> String -- ^ Source of the text (stdin/file name)
>          -> Text -- ^ "Data.Text.Lazy"'s 'Text' of the source
>          -> Either ParseError [[String]] -- ^ Either "Text.Parsec"'s 'ParseError'
>                                          --   or a 2D list of parsed strings
> parseCsv sepChar = parse (csvFile sepChar)

> newLines :: String
> newLines = "\n\r"
> quoteChars :: String
> quoteChars = "'\""

> csvFile :: Char -> SParser [[String]]
> csvFile sepChar = endBy (line sepChar) eol

> line :: Char -> SParser [String]
> line sepChar = sepBy (cell sepChar) (char sepChar)
> cell :: Char -> SParser String
> cell sepChar = quotedCell <|> many (noneOf (sepChar:newLines))
> {-# INLINE cell #-}

> quotedCell :: SParser String
> quotedCell = do
>        sChar <- oneOf quoteChars
>        content <- many (fmap return nonEscape <|> escape) --(quotedChar sChar)
>        _ <- char sChar <?> "quote at end of cell"
>        return $ concat content
> {-# INLINE quotedCell #-}

 quotedChar :: Char -> SParser Char
 quotedChar csep =
        noneOf [csep]
    <|> try (string (replicate 2 csep) >> return csep)

> nonEscape :: SParser Char
> nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"
> {-# INLINE nonEscape #-}

> escape :: SParser String
> escape = do
>   d <- char '\\'
>   c <- oneOf "\\\"0nrvtbf"
>   return [d,c]
> {-# INLINE escape #-}

> eol :: SParser String
> eol =   try (string newLines)
>     <|> try (string (reverse newLines))
>     <|> string "\n"
>     <|> string "\r"
>     <?> "end of line"
