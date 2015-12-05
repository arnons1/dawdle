

> module Database.Dawdle.Options
>  (Options(..)
>  ,getOpts
>  ,defaultOptions)
> where
>
> import           System.Console.GetOpt
> import           Text.Read ( readMaybe )
>
> data Options = Options
>  { optVerbose     :: Bool
>  , optInput       :: Maybe FilePath
>  , optStopAfter   :: Maybe Int
>  , optWithHeader  :: Bool
>  , optSepChar     :: Char
>  , optVerifyIntegrity :: Bool
>  } deriving Show

> defaultOptions :: Options
> defaultOptions    = Options
>  { optVerbose     = False
>  , optInput       = Nothing
>  , optStopAfter   = Just 10000
>  , optWithHeader  = False
>  , optSepChar     = ','
>  , optVerifyIntegrity = True
>  }
>
> options :: [OptDescr (Options -> Options)]
> options =
>   [Option ['v']     ["verbose"]
>      (NoArg (\ opts -> opts { optVerbose = True }))
>      "chatty output on stderr"
>   ,Option ['f']     ["input"]
>      (ReqArg (\ f opts -> opts { optInput = Just f }) "FILE")
>      "input FILE"
>   ,Option ['t']     ["threshold"]
>      (ReqArg (\ d opts -> opts { optStopAfter = readMaybe d :: Maybe Int}) "INT")
>      "stop after how many lines"
>   ,Option ['h']     ["with-header"]
>      (NoArg (\ opts -> opts { optWithHeader = True }))
>      "is there a header line"
>   ,Option ['s']     ["separator"]
>      (ReqArg (\d opts -> opts { optSepChar = headErr' d }) "CHAR")
>      "Separator char"
>   ,Option [] ["skip-verify-integrity"]
>      (NoArg (\ opts -> opts { optVerifyIntegrity = False }))
>      "Skip integrity verification of CSV file - all lines have the same number of columns"
>   ]

> headErr' :: [a] -> a
> headErr' (a:_) = a
> headErr' _ = error "Head error in Options.lhs"

> getOpts :: [String] -> IO (Options, [String])
> getOpts argv =
>   case getOpt Permute options argv of
>     (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
>     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
>   where
>     header = "Usage: dawdle [OPTION...] files..."
