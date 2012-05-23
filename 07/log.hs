import Data.List

data LogType = Info | Warning | Error Int deriving Eq

instance Show LogType where
    show Info = "I"
    show Warning = "W"
    show (Error l) = "E " ++ show l

instance Ord LogType where
    compare (Error l) (Error l') = compare l l'
    compare (Error _) _ = GT
    compare _ (Error _) = LT
    compare Warning Info = GT
    compare Info Warning = LT
    compare _ _ = EQ

type TimeStamp = Int
data LogMessage = LogMessage LogType TimeStamp String | Unknown String
    deriving Eq

instance Show LogMessage where
    show (LogMessage t s m) = show t ++ " " ++ show s ++ m
    show (Unknown m) = m

instance Ord LogMessage where
    compare (Unknown _) (Unknown _) = EQ
    compare (Unknown _) _ = LT
    compare _ (Unknown _) = GT
    compare (LogMessage t s _) (LogMessage t' s' _) = case compare t t' of
        EQ -> compare s' s
        r -> r

parseLogMsg :: String -> LogMessage
parseLogMsg ('E':' ':xs) | (l, ' ':xs1):_ <- reads xs
                         , (t, ' ':xs'):_ <- reads xs1
                         , l >= 1 && l <= 99          = LogMessage (Error l) t xs'
parseLogMsg ('I':' ':xs) | (t, ' ':xs'):_ <- reads xs = LogMessage Info t xs'
parseLogMsg ('W':' ':xs) | (t, ' ':xs'):_ <- reads xs = LogMessage Warning t xs'
parseLogMsg xs = Unknown xs

instance Read LogMessage where
    readsPrec _ s = [(parseLogMsg s, "")]

main = readFile "error.log" >>= writeFile "out.log" . unlines . map show . process . map read . lines
  where process :: [LogMessage] -> [LogMessage]
        process = sortBy (flip compare)