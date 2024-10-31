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
parseBrackets code = goParseBrackets code 0 [] (fromList [])
    where
        goParseBrackets :: String -> Int -> [Int] -> Map Int Int -> Maybe (Map Int Int)
        goParseBrackets [] _ [] m = Just m
        goParseBrackets [] _ _ _ = Nothing
        goParseBrackets (c:cs) i stack m
            | c == '[' = goParseBrackets cs (i + 1) (i:stack) m
            | c == ']' = case stack of
                [] -> Nothing
                (j:stack') -> goParseBrackets cs (i + 1) stack' (insert i j (insert j i m))
            | otherwise = goParseBrackets cs (i + 1) stack m

run :: String -> Map Int Int -> IO ()
run code bracketMap = goRun code 0 0 (fromList []) bracketMap
    where
        goRun :: String -> Int -> Int -> Map Int Int -> Map Int Int -> IO ()
        goRun code i _ _ _
            | i == length(code) = return ()
        goRun code i p m bracketMap
            | code!!i == '>' = goRun code (i + 1) (p + 1) m bracketMap
            | code!!i == '<' = goRun code (i + 1) (p - 1) m bracketMap
            | code!!i == '+' = goRun code (i + 1) p (insert p ((findWithDefault 0 p m) + 1) m) bracketMap
            | code!!i == '-' = goRun code (i + 1) p (insert p ((findWithDefault 0 p m) - 1) m) bracketMap
            | code!!i == '.' = do
                putChar (toEnum (m ! p))
                goRun code (i + 1) p m bracketMap
            | code!!i == ',' = do
                isEof <- hIsEOF stdin
                if isEof
                    then goRun code (i + 1) p (insert p 0 m) bracketMap
                    else do
                        c' <- getChar
                        goRun code (i + 1) p (insert p (fromEnum c') m) bracketMap
            | code!!i == '[' = if (findWithDefault 0 p m) == 0
                then goRun code (bracketMap ! i) p m bracketMap
                else goRun code (i + 1) p m bracketMap
            | code!!i == ']' = if (findWithDefault 0 p m) /= 0
                then goRun code (bracketMap ! i) p m bracketMap
                else goRun code (i + 1) p m bracketMap
            | otherwise = goRun code (i + 1) p m bracketMap