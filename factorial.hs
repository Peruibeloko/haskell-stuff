fact :: Int -> Int
fact n = factInternal n 1

factInternal :: Int -> Int -> Int
factInternal 0 acc = acc
factInternal n acc = fact (n - 1, acc * n)

main :: IO ()
main = do
  print $ fact 5