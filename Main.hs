import Data.Map (Map, findWithDefault, fromList, insert, (!))
import Data.Typeable (typeOf)
import System.Environment (getArgs)
import System.IO (hIsEOF, readFile, stdin)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            code <- readFile fileName
            case parseBrackets code of
                Just bracketMap -> run code bracketMap
                Nothing -> fail "Unbalanced brackets"
        _ -> putStrLn "Usage: runhaskell Main.hs <filename>"

parseBrackets :: String -> Maybe (Map Int Int)
parseBrackets code = go code 0 [] (fromList [])
    where
        go :: String -> Int -> [Int] -> Map Int Int -> Maybe (Map Int Int)
        go [] _ [] m = Just m
        go [] _ _ _ = Nothing
        go (c:cs) i stack m
            | c == '[' = go cs (i + 1) (i:stack) m
            | c == ']' = case stack of
                [] -> Nothing
                (j:stack') -> go cs (i + 1) stack' (insert i j (insert j i m))
            | otherwise = go cs (i + 1) stack m

run :: String -> Map Int Int -> IO ()
run code bracketMap = go code 0 0 (fromList [])
    where
        go :: String -> Int -> Int -> Map Int Int -> IO ()
        go code i _ _
            | i == length(code) = return ()
        go code i p m
            | code!!i == '>' = go code (i + 1) (p + 1) m
            | code!!i == '<' = go code (i + 1) (p - 1) m
            | code!!i == '+' = go code (i + 1) p (insert p ((findWithDefault 0 p m) + 1) m)
            | code!!i == '-' = go code (i + 1) p (insert p ((findWithDefault 0 p m) - 1) m)
            | code!!i == '.' = do
                putChar (toEnum (m ! p))
                go code (i + 1) p m
            | code!!i == ',' = do
                isEof <- hIsEOF stdin
                if isEof
                    then go code (i + 1) p (insert p 0 m)
                    else do
                        c' <- getChar
                        go code (i + 1) p (insert p (fromEnum c') m)
            | code!!i == '[' = if (findWithDefault 0 p m) == 0
                then go code (bracketMap ! i) p m
                else go code (i + 1) p m
            | code!!i == ']' = if (findWithDefault 0 p m) /= 0
                then go code (bracketMap ! i) p m
                else go code (i + 1) p m
            | otherwise = go code (i + 1) p m