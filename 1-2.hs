import Data.Set as Set

parse :: String -> Integer
parse ('+':n) = read n
parse ('-':n) = negate (read n)

loop freq seen = do
  mod <- fmap parse getLine
  let freq' = freq+mod
  if freq' `Set.member` seen then print freq' else do
    loop freq' (Set.insert freq' seen)

main = loop 0 (Set.singleton 0)
