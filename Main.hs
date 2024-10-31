import Data.Map (Map, fromList, insert)
import System.Environment (getArgs)
import System.IO (readFile)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            code <- readFile fileName
            bracketMap <- case parseBrackets code of
                Just m -> return m
                Nothing -> fail "Unbalanced brackets"
            putStrLn code
            print bracketMap
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
                (j:stack') -> goParseBrackets cs (i + 1) stack' (insert j i m)
            | otherwise = goParseBrackets cs (i + 1) stack m