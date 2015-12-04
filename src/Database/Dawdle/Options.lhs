> module Database.Dawdle.Options
>  (Options(..)
>  ,getOpts)
> where
>
> import System.Console.GetOpt
> import Data.Maybe ( fromMaybe )
> import Text.Read ( readMaybe )
>
> data Options = Options
>  { optVerbose     :: Bool
>  , optInput       :: Maybe FilePath
>  , optStopAfter   :: Integer
>  , optWithHeader  :: Bool
>  } deriving Show

> defaultOptions :: Options
> defaultOptions    = Options
>  { optVerbose     = False
>  , optInput       = Nothing
>  , optStopAfter   = 10000
>  , optWithHeader  = False
>  }
>
> options :: [OptDescr (Options -> Options)]
> options =
>   [Option ['v']     ["verbose"]
>      (NoArg (\ opts -> opts { optVerbose = True }))
>      "chatty output on stderr"
>   ,Option ['f']     ["input"]
>      (OptArg ((\ f opts -> opts { optInput = Just f }) . fromMaybe "input")
>              "FILE")
>      "input FILE"
>   ,Option ['t']     ["threshold"]
>      (ReqArg (\ d opts -> opts { optStopAfter = (fromMaybe (1000 :: Integer) (readMaybe d :: Maybe Integer))}) "INTEGER")
>      "stop after how many lines"
>   ,Option ['h']     ["with-header"]
>      (NoArg (\ opts -> opts { optWithHeader = True }))
>      "is there a header line"
>   ]
>
> getOpts :: [String] -> IO (Options, [String])
> getOpts argv =
>   case getOpt Permute options argv of
>     (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
>     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
>   where
>     header = "Usage: dawdle [OPTION...] files..."
