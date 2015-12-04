> module Database.Dawdle.Utils
>  (getFstIfJust
>  ,isCTNumber
>  ,isInt
>  ,maybeToEither
>  ,allTheSame
>  --,ppShow
>   )
> where
> import Data.Char

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
