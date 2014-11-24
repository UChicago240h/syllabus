# Lecture 1
## Haskell
As all Haskell courses begin, so does this one: with the most banal description of the language possible. **Haskell is a lazy pure functional language.** What this means, I'm assuming most of you know already: if you don't, this course is probably not for you. At the highest level, this means that Haskell is very, very, very different from most programming languages that you have worked with previously. We'll get into the details of the weirdness in a bit, with much of it hopefully being familiar.

## Today's Goals
Much of this first week is concerned with covering the groundwork necessary to understand the more advanced topics in class. We're making the assumption that folks here are familiar with Haskell and need no more than a refresher, so much of the material is going to be covered in a cursory fashion and a very rapid speed.

## Beginning with GHC(i)
Our base of operations in the Haskell world is going to be the Glasgow Haskell Compiler (GHC). While there are other Haskell compilers out there, GHC is the standard and the only thing you should ever touch. Initially, as we're experimenting and refamiliarizing ourselves with the language, we're going to spend a significant amount of time in GHCi, the interpreter facility of GHC. For readers of the notes, I've denoted lines of code to be executed within GHCi as preceded by `>` and output in GHCi with `>>>`.

Let's begin by mucking about and defining some variables:
```haskell
> let height = 5
> let width = 10
> width * height
>>> 50
> width - height
>>> 5
```
Works much as expected: nothing new to see here. More interesting is where we start going about defining functions:
```haskell
> let getArea xWdith yHeight = xWidth * yHeight
> getArea height width
>>> 50
```
Note that function invocation, as opposed to what most of us are used to from Algol-family languages, does not depend upon the use of parentheses, but properly-spaced function names followed by arguments. Let's go about and define a couple of slightly more robust functions:
```haskell
> let isLong name = (length name) > 10
> isLong "Jakub"
>>> False
> isLong "Balthazarius"
>>> True
```

Very cool. Just a quick word about GHCi before we go any further: the `let` keyword has a very different meaning in GHCi and in proper Haskell. In GHCi, we use `let` to define bindings that remain within the interpreter's scope for the duration of the session. We use this for both variables and functions. In proper Haskell, `let` is used in a very different way to denote local name bindings. We'll cover this latter use in a bit.

## Types
Haskell's type system is simultaneously its most powerful and most irritating feature. Type safety allows us to write hugely robust programs, but often at the expense of accessibility to n00bz. If you have not already, you will spend a significant amount of time tearing your hair out over type signatures. Let's take begin previous example, which we'll put in a file called *area.hs*:
```haskell
-- area.hs
getArea :: Int -> Int -> Int
getArea width height = width * height
```
Here we've defined the getArea function as before, but with the addition of a type signature. Type signatures provide an explicit type restrictions for function inputs and outputs. While the compiler can usually infer types for basic functions, type signatures provide an added layer readability for folks reading your code down the road and are critical for more advaned programs. They can also become a huge liability if not done properly. Let's load this file into GHCi and give it a whirl:
```haskell
> :l area.hs
> getArea 4 5
>>> 20
```
This works as before, but the limitations of what we've written become apparent very quickly:
```haskell
> getArea 4.0 8
>>> INCOMPREHENSIBLE ERROR MESSAGE
```
This is obviously not something we want. It should only make sense that our `getArea` function should be able to handle floating point numbers, but we've defined our function signature to handle only integers. To fix this issue, let's try removing method signature and reloading our function:
```haskell
-- area.hs
getArea width height = width * height
```
```haskell
> :l area.hs
> getArea 4 5.0
>>> 20.0
```
This is mysterious. By simply removing the type signature, something that we've created a function that works on previously failing data types and is somehow more robust than our previous, virtually identical function. Let's investigate why
```haskell
> :t getArea
>>> getArea :: Num a => a -> a -> a
```
Interesting. The interpreter has assessed the type of our function to be quite different from what initially profiled it as. The core is the same: you're working with a function taking in two arguments of the same type and returning an argument of the same type: `a -> a -> a`. But while our previous types were strictly defined as integers, GHCi has has reprofiled these types as generics. The only restriction upon these types is denoted by `Num a =>`, which means that all of these variables must be of the `Num` typeclass (more on these later). This means that all types that could be considered numbers and obey certain laws that the Haskell compiler places upon Nums are admissible in both the arguments and ouput. We can go ahead and use this information to amend our previous function defintion:
```haskell
-- area.hs
getArea :: Num a => a -> a -> a
getArea width height = width * height
```
Let's load this and make sure it works:
```haskell
> :l area.hs
> getArea 2.0 4
>>> 20.0
```
Awesome. There's plenty more to be said for this example, but we'll leave it as is for the time being. There's plenty more to be said about Haskell's type system, much of which we'll cover down the line. Truth be told, the system is so omnipresent and powerful that a lot of beginner Haskellians spend much of their time struggling within its confines. More will be said about this type system, but for now, trust me when I say that it is worth taking the time to master this system and really understand how your types are functioning.

## Control Flow
Haskell's take on control flow far outstrips what we're used to seeing with the standard Algol-family *if-then-else* constructs. Haskell provides us with a number of takes on control flow that encompass function overloading, case statements, and the standard *if-then* constructs that we're used to. Let's jump in with pattern matching.

### Pattern Matching
Haskell's pattern matching capabilities may be most analogous for most folks to Java's function overloading capabilities, but in reality are far more similar to the list deconstruction capabilities found in Lisp (i.e. Common Lisp, Racket, Scheme, Clojure, etc.). Let's take a look by defining a greeting function which properly handles folks we know, dislike, and don't know:
```haskell
-- greeting.hs
greeting :: String -> String
greeting "Henry" = "Hey Henry, let's go drinking today!"
greeting "Hazel" = "Hey Hazel, are we actually working on OS today?"
greeting "Laura" = "Hey Laura, we should just probably start that project today!"
greeting _ = "I don't know you bro."
```
```haskell
> :l greeting.hs
> greeting "Henry"
>>> "Hey Henry, let's go drinking today!"
> greeting "Shaan"
>>> "I don't know you bro."
```
As you can see, we're creating instances of our function that have different behaviors upon explicitly defined input patterns. Truth be told, there's no need to be *this* explicit about our inputs: we'll see how we can have more generalizable pattern matching once we start working with lists. One thing that should be noted is the last case. We utilize the `_` character to denote a general wildcard pattern. This takes the place of an `otherwise`, or `else` pattern.

We can use this in a slightly smarter way. Let's say we wanted to pattern-match for individuals whose names start with an 'S', simply so we can ostracize them. We can do this using a clever pattern-matching construct:
```haskell
-- ostracize.hs
ostracize :: String -> String
ostracize ('S':_) = "Only fools have a name that starts with S!"
ostracize ('J':_) = "Ha! John? Jacob? Jingleheimer? More like Jerk!"
ostracize _ = "I don't have time for you bro."
```

### Guards
Pattern matching is well and good, but oftentimes we want to have conditionals more conveniently nested in our function, rather than the top-level routing that pattern matching provides. For example, let's take a function that tells me whether or not I should stop drinking
```haskell
keepDrinking :: Int -> String
keepDrinking numDrinks
  | numDrinks < 3 = "Keep on chugging"
  | numDrinks < 6 = "You should probably slow down"
  | numDrinks < 9 = "You should probably stop"
  | otherwise     = "You should probably call an ambulance"
```
Here, we're creating more explicit conditional statements that map to program outputs. Rather than the pattern matching, we're able to create conditional statements that allow for true/false evaluation within 

### List Comprehensions
List comprehensions are one of the most efficient ways we have available to us of constructing lists. If you're a serious Pythonista, you've probably seen these once or twice, but Haskell's take on them is much closer to the way that set construction is dealt with in math:
```haskell
> [x*2 | x <- [1..20]]
>>>[2,4,6,8,10,12,14,16,18,20]
```
As you can see, this takes each element within the range `[1..10]`, doubles it, and adds it to the list. We can spice this up by adding conditional predicates to this:
```haskell
> [x*2 | x <- [1..20], x*2 >= 12]
>>>[12,14,16,18,20]
```
We can further spice this up by having multiple predicates:
```haskell
> [x | x <- [10..20], x /= 13, x /= 15, x /= 19]
>>>[10,11,12,14,16,17,18,20]
```
And we can also perform comprehensions over several lists:
```haskell
> [ x*y | x <- [2,5,10], y <- [8,10,11]]
>>> [16, 20,22,40,50,55,80,100,110]
```

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

## Lists
Haskell's take on lists is quite a bit different than what we're used to in imperative languages. Firstly, access time is not constant, but linear. Additionally, lists are constructed in a way that iterative traversal is impossible, forcing us to rely on recursion. Let's take a look at some of these details:
```haskell
> let lostNumbers = [4,8,15,16,23,42]
> lostNumbers
>>> [4,8,15,16,23,42]
```
Awesome. Let's now construct this list in a completely different way, using the const `:` operator:
```haskell
> let newNumbers = (4:(8:(15:(16:(23:(42:[]))))))
> newNumbers
>>> [4,8,15,16,23,42]
```
As we can see here, lists are really successive applications of a binary, left-associative appending operator, thus suggesting that much of our list traversal is going to involve peeling elements from the beginning of a list, rather than the iterative type of traversal.

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

## Where/Let Clauses

## Recursion

## Zip

## Map

## Fold

## Algebraic Data Types
days of week/pattern matching against this,
defining trees 
## Typeclasses
similar types == similar vocabularies
allow us to capture vocaburlaries

## Why Haskell?
With most Computer Science courses, the reason for taking it is generally fairly obvious: Why take Databases, Networks, Operating Systems, or Parallel Programming? Because these are the fundamental building blocks of systems programming. Why take Discrete Math and Algorithms? Because you need to know the fundamentals of reasoning about program runtime and performance. Why take Complexity? Because you need to graduate. But with a topic this seemingly esoteric, it is probably worthwhile to take a moment and think about why we're doing this.

Initially, our motivation for teaching this course was to be able to explore more advanced topics in Haskell and further cultivate the Haskell community at the university. This is all well and good, but it only goes so far in convincing folks that this is a good idea.

Haskell is a pretty 