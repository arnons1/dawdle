> module Database.Dawdle.Utils
>  (getFstIfJust
>  ,isCTNumber
>  ,isInt
>  ,maybeToEither
>  ,allTheSame
>  ,normalizeNames
>   )
> where
> import Data.Char
> import Data.List

> getFstIfJust :: (a, Maybe b) -> Maybe a
> getFstIfJust (a,Just _) = Just a
> getFstIfJust _ = Nothing

> isCTNumber :: String -> Bool
> isCTNumber = all (\x -> isNumber x || x `elem` ['-','.'])

> isInt :: String -> Bool
> isInt = all (\x -> isNumber x || x =='-')

> maybeToEither :: String -> Maybe a -> Either String a
> maybeToEither _ (Just x) = Right x
> maybeToEither s Nothing = Left s

> allTheSame :: (Eq a) => [a] -> Bool
> allTheSame xs = and $ map (== head xs) (tail xs)

> normalizeNames :: [String] -> [String]
> normalizeNames = map normalize
>  where
>    normalize = map toLower . (filter (\x -> x=='_' || isAlphaNum x)) . intercalate "_" . words
