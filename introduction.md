# Lecture 1: Introduction
## Why?
I'd like to begin today by offering an argument for this course's existence. There is a relatively large and unreasonably vocal contingent of Haskell folks at this institution, and a relatively small and equally vocal contingent at every major technology firm, but having fans does not make something worthy of academic study (regardless of what [certain folks at this university think](http://www.nytimes.com/2011/10/31/arts/television/jersey-shore-has-its-day-at-university-of-chicago.html)). Haskell, despite its cachet in hipster cliques, does not have a noticeable footprint in the world of commerce (only [one company](https://www.sc.com/en/) that I know of has a significant amount of its codebase written in Haskell). On top of that, while many of Haskell's libraries are quite delightful, few of them are what I would call "production ready". (As an aside, if you are interested in using Haskell in production, I would recommend you watch [Bryan O'Sullivan's talk on running his Haskell-based startup](https://www.youtube.com/watch?v=ZR3Jirqk6W8).) In academia, Haskell usage is mostly limited to the programming languages sector of any given department, and even then, Haskell folks aren't particularly common. In the open-source community, there is a good amount of Haskell activity, though, yet again, few of the projects coming out of the community receive widespread adoption or even stability.

So the question remains: why dedicate a large chunk of your time and effort into mastering a language that is wildly obscure and has a reputation for being completely opaque, especially when its impact is so seemingly limited? While answers like "It's cool" or "It's a lot of fun!" or (in the case of 161 students) "I need to for this degree", these answers really aren't going to cut it for this class. Over the next several months, we'll work towards constructing a more nuanced and powerful argument for Haskell's existence, but for this session, we'll turn to one of the masters. In 2007, Simon Peyton-Jones authored a [terrific paper on Haskell's history](http://research.microsoft.com/en-us/um/people/simonpj/papers/history-of-haskell/history.pdf) (it's highly recommended reading). In it, there is one line that sticks out to me: "We believe the most important legacy of Haskell will be how it influences the languages that succeed it." The grander argument that I'd like to put forth is that the ideas underlying Haskell are currently influencing the way programs are written and architected and form the backbone for the philosophy surrounding modern technology stacks. We'll fill in the details as we go along.

## Today's Goals
Much of this first week is concerned with covering the groundwork necessary to jump into more advanced topics. We're making the assumption that folks here are familiar with Haskell and need no more than a refresher, so much of the material is going to be covered in a cursory fashion and a very rapid speed. Should that not be the case for you, office hours are a resource that should be taken advantage of. In addition, I'll be hosting a number of bootcamp sessions outside of regular office hours where we'll be working through my favorite technical book of all time, Miran Lipovača's peerless [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/).

## Haskell
As all Haskell courses begin, so does this one: with the most banal description of the language possible. **Haskell is a lazy pure functional language with a strong, static type system.** What this means, I'm assuming most of you know already: if you don't, we'll work to rectify this outside of class. At the highest level, this means that Haskell is very, very, very different from most programming languages that you have worked with previously. We'll spent the better part of this lecture unraveling these things.

## Beginning with GHC(i)
Our base of operations in the Haskell world is going to be the Glasgow Haskell Compiler (GHC). While there are other Haskell compilers out there, GHC is the standard and the only thing you should ever touch. Initially, as we're experimenting and refamiliarizing ourselves with the language, we're going to spend a significant amount of time in GHCi, the interpreter facility of GHC. For readers of the notes, I've denoted lines of code to be executed within GHCi as preceded by `>` and output in GHCi with no preceding symbol.

Let's begin by mucking about and defining some variables:
```haskell
> let height = 5
> let width = 10
> width * height
50
> width - height
5
```
Works much as expected: nothing new to see here. More interesting is where we start going about defining functions:
```haskell
> let getArea xWdith yHeight = xWidth * yHeight
> getArea height width
50
```
Note that function invocation, as opposed to what most of us are used to from Algol-family languages, does not depend upon the use of parentheses, but properly-spaced function names followed by arguments. Let's go about and define a couple of slightly more robust functions:
```haskell
> let isLong name = (length name) > 10
> isLong "Jakub"
False
> isLong "Balthazarius"
True
```

Very cool. Just a quick word about GHCi before we go any further: the `let` keyword has a very different meaning in GHCi and in proper Haskell. In GHCi, we use `let` to define bindings that remain within the interpreter's scope for the duration of the session. We use this for both variables and functions. In proper Haskell, `let` is used in a very different way to denote local name bindings. We'll cover this latter use in a bit.

## Types
Haskell's type system is simultaneously its most powerful and most irritating feature for newcomers. Type safety allows us to write hugely robust programs, but often at the expense of accessibility to n00bz.

In order to get ourselves mentally prepared to accept Haskell's type system, let's take a moment and rip on less well-designed languages. Let's talk about JavaScript. Let's try a couple of exercises. JavaScript, a dynamically typed language, allows us to do all kinds of shenanigans with basic arithmetic operators. Some of these are actually pretty nifty:
```javascript
$ node
> 'a' * 16
'aaaaaaaaaaaaaaaa'
```
But our experimentation quickly leads us down some ugly roads. Take adding two empty arrays:
```javascript
> [] + []
''
```
So adding two arrays gives as empty string. That doesn't seem right.... But one of the core properties of addition is commutativity, i.e. $a + b = b + a$. Hence, adding an array to an object should give us the same result, regardless of the addition order:
```javascript
> [] + {}
'[object Object]'
> {} + []
0
```
Right. As we can see, JavaScript's unfortunate brand of dynamic typing provides us with some unexpected behavior. (While I hold no opinions on Brendan Eich's politics, I do have a pretty poor opinion of him as a language designer.) While this behavior can be pretty funny (see [Gary Bernhardt's lightning talk for proof](https://www.destroyallsoftware.com/talks/wat)), this becomes a huge issue when trying in debugging and ensuring the security application. Take it from someone who has spent hours trying to figure out the return type of a Python function: types matter.

But to those of us used to running rampant with a forgiving type system, Haskell's type system presents an unusual challenge. Let's begin affirming this by trying some of the above shenanigans in GHCi:
```haskell
> 'a' * 3
No instance for (Num Char) arising from a use of ‘*’
  In the expression: 'a' * 3
  In an equation for ‘it’: it = 'a' * 3
```
While Haskell's errors are generally just left of incomprehensible, this one's pretty clear: you can't multiply `Char`s and `Int`s. This brings us to our first lesson: Haskell does not allow for function overloading that allows for wildly disparate behavior and return types across a variety of function inputs. Haskell approaches generic programming in a much more disciplined and effective way, which will explore in due time.

Haskell's type system also allows for clear and effective use of code as documentation. Let's take our previosu function defintion example, which we'll put in a file called *area.hs*:
```haskell
-- area.hs
getArea :: Int -> Int -> Int
getArea width height = width * height
```
Here we've defined the getArea function as before, but with the addition of a type signature. Type signatures provide an explicit type restrictions for function inputs and outputs. While the compiler can usually infer types for basic functions, type signatures provide an added layer readability for folks reading your code down the road and are critical for more advanced programs. They can also become a huge liability if not done properly. Let's load what we've written into GHCi and give it a whirl:
```haskell
> :l area.hs
> getArea 4 5
20
```
This works as before, but the limitations of what we've written become apparent very quickly:
```haskell
> getArea 4.0 8
INCOMPREHENSIBLE ERROR MESSAGE
```
This is obviously not something we want. It should only make sense that our `getArea` function should be able to handle floating point numbers, but we've defined our function signature to handle only integers. To fix this issue, let's try removing method signature and reloading our function:
```haskell
-- area.hs
getArea width height = width * height
```
```haskell
> :l area.hs
> getArea 4 5.0
20.0
```
This is mysterious. By simply removing the type signature, something that we've created a function that works on previously failing inputs and is somehow more robust than our previous, virtually identical function. Let's investigate why:
```haskell
> :t getArea
getArea :: Num a => a -> a -> a
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
20.0
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
"Hey Henry, let's go drinking today!"
> greeting "Shaan"
"I don't know you bro."
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

## Local Bindings
While the Haskell way is to prevent data from being assigned to temporary placeholders, sometimes we do need to alias certain data points to keep our code clean. Take the following example:
```haskell
-- bmi.hs
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | weight / height ^ 2 <= 18.5  = "You're underweight!"
  | weight / height ^ 2 <= 25.0  = "You're supposedly normal."
  | weight / height ^ 2 <= 30.0  = "You're fat!"
  | otherwise                    = "You're a whale!"
```
We can see that we repeat the `weight / height ^ 2` clause three separate times, so let's see if we can't alias that away:
```haskell
-- bmi.hs
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5  = "You're underweight!"
  | bmi <= 25.0  = "You're supposedly normal."
  | bmi <= 30.0  = "You're fat!"
  | otherwise    = "You're a whale!"
  where bmi = weight / height ^ 2
```
Awesome, that simplifies our code hugely. Now let's further alias the numerical values here to make a bit more sense of them:
```haskell
-- bmi.hs
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny  = "You're underweight!"
  | bmi <= normal  = "You're supposedly normal."
  | bmi <= fat     = "You're fat!"
  | otherwise      = "You're a whale!"
  where bmi = weight / height ^ 2
  	skinny = 18.5
	normal = 25.0
	fat = 30.0
```
Awesome, our function is a lot clearer and more readable thanks to our `where` bindings. Now, while this is a 

## Recursion
The vast majority of CS students (at other, more sane institutions) learned about looping by way of the glorious `for` and `while` loops. But we're insufferable hipsters, so we learned about recursion. Since Haskell doesn't allow us to mutate variables or global state, we are pretty much limited to working with various forms of traversable objects (`Functor`, `Foldable`, `Traversable`, etc.). Recursion works pretty much exactly like you'd expect it to:
```haskell
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)
```
Now, while this is unsurprising, it is inefficient. As in most basic fibonacci implementations, we're recomputing an enormous amount of values, thus causing a huge performance hit. Just to illustrate, let's try actually running this:

```haskell
-- fibonacci.hs
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

main = do
  print $ fibonacci 40
```

```bash
$ ghc fibonacci.hs
$ time ./fibonacci
102334155

real  0m10.601s
user  0m10.560s
sys   0m0.047s
```

At this point, we're taking slightly north of 10 seconds for calculating a fibonacci number. Suffice it to say, this will not scale. But we know we can improve this with some basic memoization. Just to make sure we're all on the same page, much of the issue here stems from the fact that we're recursively recalculating multiple fibonacci values. We can solve this by caching our intermediary values in some kind of data structure that supports constant-time lookup, thus allowing us to skip the recalculation steps that are adding so much to the runtime.  

Unfortunately, memoization in imperative languages generally involves mutating some global data structure and then performing lookups and inserts based on the status of that global data structure. As an example, let's consider the memoized problem in Python:
```python
fib_vals = {}
def fibonacci_memo(n):
  if n == 0:
    return 0
  if n == 1:
    return 1
  if n in fib_vals:
    return fib_vals[n]
  fib_vals[n] = fibonacci(n-1) + fibonacci(n-2)
  return fib_vals[n]
```
As you can see, the entire memoization procedure depends upon our mutation of a globally-accessible data structure. While this is all well and good, we really can't do this in Haskell, thus forcing us to be a more creative. Before we can make this happen, we need to look back and recall the `map` function.

## Map
One of the beautiful things about Haskell is how we can utilize higher-order functions. In most imperative languages that we're familiar with, functions only go so far as their explicit definition, and are extensible only to the extent of their type signatures, if even that. Haskell allows us a whole host of magical capacities to play with functions. The first of these is the ability to curry functions, that is to create partially applied functions. As a result of this, we can create functions that are effectively function generators:
```haskell
mulFunc :: Num a => a -> a -> a
mulFunc a = (* a)

> let double = mulFunc 2
> double 4
8
```
Awesome, so we can create functions that generate more functions. Now, let's say we want to take this function and apply it to multiple arguments within a list. In order to make this happen, we can create a function that takes in said list and function, and sequentially applies the function to each element within the list. As such, we can write a map function in the canonical Haskell way:
```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
```
For those of you that haven't seen this before, note that all we're doing is pattern matching and generating sequential recursive applications via list decomposition. As far as using this, let's say we wanted to take a list and double all elements within it:
```haskell
> map (*2) [2..10]
[4,6,8,10,12,14,16,18,20]
```

Fairly basic stuff: we've managed to double each number in the list by mapping over it. Let's try a couple more things:
```haskell
> map (++ "!") ["BIFF", "BANG", "POW"]
["BIFF!", "BANG!", "POW!"]
> map (replicate 3) [3..6]
[[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
[[1,4],[9,16,25,36],[49,64]]
> map (!! 1) [[1,2],[3,4,5,6],[7,8]]
[2,4,8]
```
Do note the `!!` operator. This operator defines explicit indexing in Haskell. This is far less commonly used in Haskell than many other languages, since array indexing is actually an `O(n)` operation, rather than the `O(1)` operation we're used to.

## Memoizing
Now that we've gotten a good understanding of maps, let's go ahead and think about how we can utilize this for memoization. While we can't create mutable data structures, we can create a recursively defined list that effectively caches intermediary values, and then map our fibonacci function over that list. We can even make this an infinite list, since, due to Haskell's lazy evaluation, only the values up to the value of interest will actually be evaluated.
```haskell
-- fibonacci_memo
fibonacci :: Int -> Integer
fibonacci = (map fib [0..] !!)
  where fib 0 = 0
        fib 1 = 1
        fib n = fibonacci (n-1) + fibonacci (n-2)

main = do
  print $ fibonacci 40
```
Note both the use of the `!!` operator, and the `[0..]` infinite range. As we mentioned before, Haskell's lazy evaluation allows us to utilize such infinite ranges without loss of performance. The locally defined `fib` function, combined with the map, allows us to get our runtime down from `O(n!)` to `O(n)`. We can see that results by explicitly timing our program:

```bash
$ ghc fibonacci_memo.hs
$ time ./fibonacci_memo
102334155

real  0m0.002s
user  0m0.000s
sys   0m0.000s
```
This, to say the least, is a not-insignificant improvement. But we can still do better. Let's consider...

## Data Types
Let's finish up by pushing our fibonacci calculator as far as we possibly can. Right now, we've gotten our runtime down to `O(n)`, but there is a way to further shave our runtime down to `O(log n)`.

If we write the equations F_1 = F_1 and F_2 = F_0 + F_1 in matrix notation, we get:
```
( F_1 F_2 ) = (0 1; 1 1) * (F_0; F_1)
( F_2 F_3 ) = (0 1; 1 1) * (F_1; F_2) = (0 1; 1 1)^2 * (F_0; F_1) 
```
More generally, we have:
```
( F_n F_n+1 ) = (0 1; 1 1)^n * (F_0; F_1) 
```
Now, we can use fast exponentiation to break down our calculating `f^n` in `O(log n)` time. As an example, let's see this via python:
```python
def pow(base, power):
  if power == 0:
    return 1
  if power % 2 == 0:
    return pow(base*base, power/2)
  return base * pow(base, power-1)
```
Due to recursive halving of the power, we're able to get this runtime down to `O(log n)` runtime. Now, let's combine this with our initial insight about exploiting matrix multiplication to get a close-to-ideal version of fibonacci:

First, we need to define a matrix datatype. We can easily do this by way of Haskell's ability to define datatypes:
```haskell
data GL2 = GL2 Integer Integer Integer Integer
```

Now, let's define a matrix multiplication routine:
```haskell
mul :: GL2 -> GL2 -> GL2
mul (GL2 a b c d) (GL2 e f g h) = GL2 (a*e+b*g) (a*f+b*h) (c*e+d*g) (c*f+d*h)
```

We can now utilize this machinery to create a recursive definition of recursive exponentiation:
```haskell
fastexp :: GL2 -> Int -> GL2
fastexp _ 0 = GL2 1 0 0 1
fastexp a 1 = a
fastexp a n
  | even n = fastexp (mul a a) (div n 2)
  | otherwise = mul a (fastexp a (n-1))
```

We now have our recursive definition dividing the power by 2 at each point, while squaring the matrix at each point there's an even power. Now, we just need to add on the machinery and run the program:
```haskell
fib :: Int -> Integer
fib n = b
  where GL2 a b _ _ = fastexp (GL2 1 1 1 0) n

main = do
  print $ fib 40
```

Now let's run and time the program:
```bash
$ ghc fibonacci_matrix.hs
$ time ./fibonacci_matrix

real   0m0.004s
user   0m0.000s
sys    0m0.003s
```
So the actual runtime is slightly greater than our memoized version, most likely due to the overhead of having to calculate matrix multiplications. But, algorithmically, we can't get much better than this.

Next time, we'll be covering the Typeclassopaedia, so be ready for monads, functors, and applicatives.
