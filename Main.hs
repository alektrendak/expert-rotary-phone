import System.Environment (getArgs)
import System.IO (readFile)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            contents <- readFile fileName
            putStrLn contents
        _ -> putStrLn "Usage: runhaskell Main.hs <filename>"
