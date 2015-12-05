
> module Database.Dawdle.Utils
>  (isCTNumber
>  ,isInt
>  ,maybeToEither
>  ,allTheSame
>  ,normalizeNames
>   )
> where
> import Data.Char
> import Data.List

> -- | Is this a number or a float(y) number? (Can give incorrect
> --   results, for example with version numbers like 0.5.1.1)
> isCTNumber :: String -> Bool
> isCTNumber = all (\x -> isNumber x || x `elem` ['-','.'])

> -- | Is this number an int? (Similar to 'isCTNumber', but without a decimal point)
> isInt :: String -> Bool
> isInt = all (\x -> isNumber x || x =='-')

> -- | Promote a Maybe to an Either with an error string
> maybeToEither :: String -- ^ Error string
>               -> Maybe a -- ^ Maybe value to check
>               -> Either String a -- ^ Either Error string or the Just value from the maybe
> maybeToEither _ (Just x) = Right x
> maybeToEither s Nothing = Left s

> -- | Check that all the items in a list are identical. This is used to verify lengths in the algorithm.
> allTheSame :: (Eq a) => [a] -> Bool
> allTheSame xs = all (== head xs) (tail xs)

> -- | Normalize names by removing special characters, replacing
> --   spaces with underscores and lowercasing everything.
> normalizeNames :: [String] -- ^ List of strings to normalize
>                -> [String] -- ^ List of normalized strings
> normalizeNames = map normalize
>  where
>    normalize = map toLower . filter (\x -> x=='_' || isAlphaNum x) . intercalate "_" . words
