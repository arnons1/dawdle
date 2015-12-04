> {-# LANGUAGE ExplicitForAll,TupleSections,
>              NoMonomorphismRestriction,OverloadedStrings,
>              FlexibleContexts, RankNTypes, ViewPatterns #-}

> import Database.Dawdle.Parser
> import Database.Dawdle.Dawdle
> import Database.Dawdle.Options
> import Database.Dawdle.Utils
> import Database.Dawdle.PrettyPrint

> import qualified Data.Text.Lazy.IO as LT
> import qualified Data.Text.Lazy as LT
> import Text.Parsec.Text ()
 
> import Data.Maybe
> import System.Environment
> import System.FilePath


> main :: IO ()
> main = do
>  a <- getArgs
>  opts <- getOpts a
>  let source = fromMaybe "stdin" $ (optInput . fst) opts
>      sepChar = (optSepChar . fst) opts
>      inputMode = (optInput . fst) opts
>      withHeader = (optWithHeader . fst) opts
>  c <- case inputMode of
>         Just f -> LT.readFile f
>         Nothing -> do
>           LT.pack <$> getContents
>  case parseCsv sepChar source c of
>    Left e -> do
>      putStrLn "Error parsing input:"
>      print e
>    Right [] -> error "Empty parsing result. Empty CSV?"
>    Right res@(x:xs) -> do
>      if withHeader
>      then if null xs
>        then error "No content after header. Empty CSV!"
>        else -- Parser rest with pretty columns
>             let x' = normalizeNames x
>             in putStrLn $ either error (pretty (genTbName source) x') $ analyzeFile xs
>      else -- No header
>        let colNms = take (length x) [ "col_"++show i | i <- [1..] :: [Int] ]
>        in putStrLn $ either error (pretty (genTbName source) colNms) $ analyzeFile res
 
>  where
>    genTbName = takeBaseName


