> {-# LANGUAGE ExplicitForAll,TupleSections,
>              NoMonomorphismRestriction,OverloadedStrings,
>              FlexibleContexts, RankNTypes #-}

> module Database.Dawdle.Parser
>        (parseCsv)
> where
> import Database.Dawdle.Types
> import Data.Text.Lazy (Text)
> import Text.Parsec

> parseCsv :: Char -> String -> Text -> Either ParseError [[String]]
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
