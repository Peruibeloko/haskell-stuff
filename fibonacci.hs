fib :: Int -> Int
fib n = fibInternal n 0

fibInternal :: Int -> Int -> Int
fibInternal 0 acc = 0 + acc
fibInternal 1 acc = 1 + acc
fibInternal n acc = fibInternal (n - 1, fibInternal (n - 2, acc))

main :: IO ()
main = do
  print $ fib 10

-- fib (5, 0)
-- fib (4, fib (3, 0))
-- fib (4, fib (2, fib (1, 0)))
-- fib (4, (fib 2, 1))
-- fib (4, (fib 1, fib (0, 1)))
-- fib (4, (fib 1, 1))
-- fib (4, 2)
-- fib (3, fib (2, 2))
-- fib (3, fib (1, fib (0, 2)))
-- fib (3, fib (1, 2))
-- fib (3, 3)
-- fib (2, fib (1, 3))
-- fib (2, 4)
-- fib (1, fib (0, 4))
-- fib (1, 4)
-- 5

-- fib (4, 0)
-- fib (3, fib (2, 0))
-- fib (3, fib (1, fib (0, 0)))
-- fib (3, 1)
-- fib (2, fib (1, 1))
-- fib (2, 2)
-- fib (1, fib (0, 2))
-- fib (1, 2)
-- 3