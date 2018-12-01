import System.IO

parse :: String -> Integer
parse ('+':n) = read n
parse ('-':n) = negate (read n)

loop acc = do
  end <- isEOF
  if end then print acc else do
    l <- getLine
    loop (acc + parse l)

main = loop 0
  
