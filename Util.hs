module Util(parseInt, IntParseError) where

newtype IntParseError = IntParseError String

parseInt :: String -> Either IntParseError Int
parseInt "" = Left (IntParseError "Empty string")
parseInt (c:cs) = case parseIntFromChar c of
    Left err -> Left err
    Right i1 -> case cs of
        "" -> Right i1
        _ -> case parseInt cs of
                Left err -> Left err
                Right i2 -> Right (i1 * 10 ^ length cs + i2)

parseIntFromChar :: Char -> Either IntParseError Int
parseIntFromChar c = case c of
    '0' -> Right 0
    '1' -> Right 1
    '2' -> Right 2
    '3' -> Right 3
    '4' -> Right 4
    '5' -> Right 5
    '6' -> Right 6
    '7' -> Right 7
    '8' -> Right 8
    '9' -> Right 9
    _ -> Left (IntParseError ("Unexpected character: " ++ [c]))