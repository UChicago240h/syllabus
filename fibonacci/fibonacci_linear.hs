fib = (fibs !!)
  where fibs = zipWith (+) (0:fibs) (0:1:fibs)

main = do
  print $ fib 40
