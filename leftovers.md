### Cases
Case expressions perform the same function as pattern-matching, but can be used in the middle of expressions, as opposed to being limited to the top level of an expression. In fact, pattern matching is merely syntactic sugar for case expressions:
```haskell
-- cases.hs
head' :: [a] -> a
head' [] = error "No head for empty lists!"
head' (x:_) = x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
	     	       	       	    	       [x] -> "a singleton list."
					       xs -> "a longer list."
```

## If/Then
Haskell does have an if-then-else construct, though it isn't used particularly often, since it is completely interchangeable with guards:
```haskell
-- ifthen.hs
describeLetter :: Char -> String
describeLetter c =
  if c >= 'a' && c <= 'z'
     then "Lower case"
     else if c >= 'A' && c <= 'Z'
     	  then "Upper case"
	  else "Not an ASCII letter"
```
Now let's look at the guards version:
```haskell
-- ifthen.hs
describeLetter :: Char -> String
describeLetter c =
  | c >= 'a' && c <= 'z' = "Lower case"
  | c >= 'A' && c <= 'Z' = "Upper case"
  | otherwise            = "Not an ASCII letter"
```
As you can probably see, the guarded verion is far cleaner and much more readable overall.
