data GL2 = GL2 Integer Integer Integer Integer

mul :: GL2 -> GL2 -> GL2
mul (GL2 a b c d) (GL2 e f g h) = GL2 (a*e+b*g) (a*f+b*h) (c*e+d*g) (c*f+d*h)

fastexp :: GL2 -> Int -> GL2
fastexp _ 0 = GL2 1 0 0 1
fastexp a 1 = a
fastexp a n
  | even n = fastexp (mul a a) (div n 2)
  | otherwise = mul a (fastexp a (n-1))

fib :: Int -> Integer
fib n = b
  where GL2 a b _ _ = fastexp (GL2 1 1 1 0) n

main = do
  print $ fib 40
