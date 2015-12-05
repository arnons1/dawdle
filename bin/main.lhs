
> {-# LANGUAGE ExplicitForAll,TupleSections,
>              NoMonomorphismRestriction,OverloadedStrings,
>              FlexibleContexts, RankNTypes #-}

> import           Database.Dawdle.Parser
> import           Database.Dawdle.Dawdle
> import           Database.Dawdle.Options
> import           Database.Dawdle.Utils
> import           Database.Dawdle.PrettyPrint

> import qualified Data.Text.Lazy.IO as LT
> import qualified Data.Text.Lazy    as LT
 
> import           Data.Maybe
> import           System.Environment
> import           System.FilePath
> import           System.IO ( hPutStrLn, stdin, stderr )

> main :: IO ()
> main = do
>  a <- getArgs
>  opts <- fst <$> getOpts a
>  let source = fromMaybe "stdin" $ optInput opts
>      sepChar = optSepChar opts
>      inputMode = optInput opts
>      withHeader = optWithHeader opts
>      stopAfter = optStopAfter opts
>  c <- case inputMode of
>         Just f -> LT.readFile f
>         Nothing -> LT.hGetContents stdin
>  c' <- case stopAfter of
>        Nothing ->
>          hPutStrLn stderr "Warning: No limit was set. Entire file will be read" >> 
>          return c
>        Just x -> return $ LT.unlines $ take x $ LT.lines c
>  case parseCsv sepChar source c' of
>    Left e -> do
>      putStrLn "Error parsing input:"
>      print e
>    Right [] -> error "Empty parsing result. Empty CSV?"
>    Right res@(x:xs) ->
>      if withHeader
>      then if null xs
>        then error "No content after header. Empty CSV!"
>        else -- Parse rest with pretty columns
>             let x' = normalizeNames x
>             in putStrLn $ either error (pretty (genTbName source) x') $ analyzeFile opts xs
>      else -- No header, parse entire file
>        let colNms = take (length x) [ "col_"++show i | i <- [1..] :: [Int] ]
>        in putStrLn $ either error (pretty (genTbName source) colNms) $ analyzeFile opts res
 
>  where
>    genTbName = takeBaseName


