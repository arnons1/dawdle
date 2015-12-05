

> module Database.Dawdle.Options
>  (Options(..)
>  ,getOpts
>  ,defaultOptions)
> where
>
> import           System.Console.GetOpt
> import           Text.Read ( readMaybe )

> -- | Options structure for passing to the main module's 'analyzeFile'
> data Options = Options
>  { optVerbose     :: Bool -- ^ Verbose mode
>  , optInput       :: Maybe FilePath -- ^ stdin (Nothing) or file to read (Just FilePath)
>  , optStopAfter   :: Maybe Int -- ^ Threshold for stopping (Just Int)
>                                --   or consume entire file (Nothing)
>  , optWithHeader  :: Bool -- ^ Is the first line a header? (Boolean)
>  , optSepChar     :: Char -- ^ Separator character for CSV (Default ',')
>  , optVerifyIntegrity :: Bool -- ^ Verify integrity of CSV file first? (Default True)
>  } deriving Show

> -- | Default set of options:
> --
> --   * Verbosity: False
> --
> --   * Input: Nothing (stdin)
> --
> --   * Stop after threshold: Just 10000
> --
> --   * With header: False
> --
> --   * Separator char: ,
> --
> --   * Verify integrity: True
> --
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

> -- | getOpts constructs a set of flags based on the user's input
> --   and the default options set in the 'defaultOptions' structure
> getOpts :: [String] -> IO (Options, [String])
> getOpts argv =
>   case getOpt Permute options argv of
>     (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
>     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
>   where
>     header = "Usage: dawdle [OPTION...] files..."
