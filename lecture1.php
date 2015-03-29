<?php
    include "../course.inc";
    $lecture='QuickCheck';
    $math=true;
    head("Zipper");
?>
<h1>Lecture 1: A Haskell Refresher</h1>
<h2>Administrivia</a>
<p>Please note material on the course <a href="http://cmsc-22311.cs.uchicago.edu/2015/">Home Page</a>.

<h2>Expectations and Goals</h2>
<p>This course is designed with the expectation that everyone here has an understanding of Haskell equivalent to completing CMSC 16100. While we will be doing a refresher of this material, it will be brief and far from equivalent to a complete, quarter-long exposure to Haskell. While we do welcome folks who are self-taught in Haskell or have managed to learn Haskell elsewhere, we do expect an understanding of and capacity in Haskell up to at least monadic computation. If you have not taken 16100 but are still interested in taking this course, please speak to either Professor Kurtz or myself sometime after class.</p>

<p>This course is designed as a follow-up to CMSC 16100, exploring topics in Haskell with the goal of creating workable software in the language. While much Haskell literature focuses on the theoretical foundations of the language, our goal here is to provide you guys with a powerful enough toolset to create applications more robust than a string of pure computations bookended by some rudimentary IO.</p>

<p>While there is a growing field of Haskell writing that focuses on writing software meant to exist in the wild, much of this writing is scattered across various blog posts that occasionally bubble up in Hacker News. As such, our secondary, and much more ambitious goal, is to create some kind of unified resource for advanced, systems-driven programming in Haskell. We hope that this class can begin serving this end. Due to the experimental nature of this, we'd like to come into this with the expectation that much of this material is (at best) in beta mode, from both the pedagogical and software perspective. There will be rough patches, things will break, and we will get things wrong. Come into this class with a tolerance for these problems, and we will do our best to reward your patience.</p>

<h2>A Gentle Start</h2>

<p>Let's begin with the basics. Haskell is a strong, statically typed, pure functional language that is lazily evaluated. This pretty much covers most of what you need to know, but we'll spend the better part of this lecture unraveling what these statements mean.</p>

<p>Let's start with some basic computations and move into the type system. Let's jump into some examples. For these, let's boot up GHCi.</p>
<code>
> let height = 5
> let width = 10
> width * height
50
> width - height
5
</code>

<p>Nothing shocking here: we can perform basic calculations and perform what seems like variable bindings. Let's go ahead and define a function:</p>

<code>
> let area h w = h * w
> area height width
50
</code>

<p>Once again, we're not starting any revolutions here. We've defined a function. But let's start taking a look at the types of what we've defined. Before we begin, formulate what you expect the types of <code>height</code>, <code>width</code>, and <code>area</code> to be. Now, let's go ahead and check each of these out:</p>

<code>
> :t width
Num a => a
> :t height
Num a => a
> :t area
Num a => a -> a -> a
</code>

<p>Okay, that's a little unexpected. For those of us</a>
<h2>Hitting the Ground Running</h2>
<p>I'm of the belief that good problems get us to great solutions, so we're going to begin with a classic: the <strong><a href="#">maximum subarray problem</a></strong>. Formally stated, (ripped directly from Wikipedia), "the maximum subarray problem is the task of finding the contiguous subarray within a one-dimentional array of numbers (containing at least one positive number) which has the largest sum."</p>

<p>Our task is going to be to use what we've learned thus far to write a function that calculates and returns the maximum sum subarray given a list of numbers. By the end of this lecture, we'll have created a command line tool that takes a in a text file as input, finds the max-sum subarray of each line, and outputs each of these subarrays to a file.</p>

<code>
import Data.List (inits, tails, maximumBy)
import Data.Ord (comparing)

subseqs :: [a] -> [[a]]
subsequs = concatMap inits . tails

maxsubseq :: (Ord a, Num a) => [a] -> [a]
maxsubseq = maximumBy (comparing sum) . subseqs

main :: IO ()
main =  print $ maxsubseq [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1]
</code>

<code>
maxsubseq = snd . foldl f ((0,[]),(0,[])) where
        f ((h1,h2),sofar) x = (a,b) where
                a = max (0,[]) (h1 + x, h2 ++ [x])
                b = max sofar a

main = print $ maxsubseq [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1]
</code>


// conversion starts here
<h1>Lecture 1</h1>
<h2>Haskell</h2>
<p>As all Haskell courses begin, so does this one: with the most banal description of the language possible. **Haskell is a lazy pure functional language.** What this means, I'm assuming most of you know already: if you don't, this course is probably not for you. At the highest level, this means that Haskell is very, very, very different from most programming languages that you have worked with previously. We'll get into the details of the weirdness in a bit, with much of it hopefully being familiar.</p>

<h2>Today's Goals</h2>
<p>Much of this first week is concerned with covering the groundwork necessary to understand the more advanced topics in class. We're making the assumption that folks here are familiar with Haskell and need no more than a refresher, so much of the material is going to be covered in a cursory fashion and a very rapid speed.</p>

<h2>Beginning with GHC(i)</h2>
<p>Our base of operations in the Haskell world is going to be the Glasgow Haskell Compiler (GHC). While there are other Haskell compilers out there, GHC is the standard and the only thing you should ever touch. Initially, as we're experimenting and refamiliarizing ourselves with the language, we're going to spend a significant amount of time in GHCi, the interpreter facility of GHC. For readers of the notes, I've denoted lines of code to be executed within GHCi as preceded by `>` and output in GHCi with `>>>`.</p>

<p>Let's begin by mucking about and defining some variables:</p>
<code>
  > let height = 5
  > let width = 10
  > width * height
  >>> 50
  > width - height
  >>> 5
</code>

<p>Note that function invocation, as opposed to what most of us are used to from Algol-family languages, does not depend upon the use of parentheses, but properly-spaced function names followed by arguments. Let's go about and define a couple of slightly more robust functions:</p>
<code>
  > let isLong name = (length name) > 10
  > isLong "Jakub"
  >>> False
  > isLong "Balthazarius"
  >>> True
</code>

<p>Very cool. Just a quick word about GHCi before we go any further: the `let` keyword has a very different meaning in GHCi and in proper Haskell. In GHCi, we use `let` to define bindings that remain within the interpreter's scope for the duration of the session. We use this for both variables and functions. In proper Haskell, `let` is used in a very different way to denote local name bindings. We'll cover this latter use in a bit.</p>

<h2>Types</h2>
<p>Haskell's type system is simultaneously its most powerful and most irritating feature. Type safety allows us to write hugely robust programs, but often at the expense of accessibility to n00bz. If you have not already, you will spend a significant amount of time tearing your hair out over type signatures. Let's take begin previous example, which we'll put in a file called *area.hs*:</p>
<code>
  -- area.hs
  getArea :: Int -> Int -> Int
  getArea width height = width * height
</code>

<p>Here we've defined the getArea function as before, but with the addition of a type signature. Type signatures provide an explicit type restrictions for function inputs and outputs. While the compiler can usually infer types for basic functions, type signatures provide an added layer readability for folks reading your code down the road and are critical for more advaned programs. They can also become a huge liability if not done properly. Let's load this file into GHCi and give it a whirl:</p>
<code>
  > :l area.hs
  > getArea 4 5
  >>> 20
</code>

<p>This works as before, but the limitations of what we've written become apparent very quickly:</p>
<code>
  > getArea 4.0 8
  >>> INCOMPREHENSIBLE ERROR MESSAGE
</code>

<p>This is obviously not something we want. It should only make sense that our `getArea` function should be able to handle floating point numbers, but we've defined our function signature to handle only integers. To fix this issue, let's try removing method signature and reloading our function:</p>

<code>
  -- area.hs
  getArea width height = width * height
</code>
<code>
  > :l area.hs
  > getArea 4 5.0
  >>> 20.0
</code>

<p>This is mysterious. By simply removing the type signature, something that we've created a function that works on previously failing data types and is somehow more robust than our previous, virtually identical function. Let's investigate why:</p>

<code>
  > :t getArea
  >>> getArea :: Num a => a -> a -> a
</code>

<p>Interesting. The interpreter has assessed the type of our function to be quite different from what initially profiled it as. The core is the same: you're working with a function taking in two arguments of the same type and returning an argument of the same type: `a -> a -> a`. But while our previous types were strictly defined as integers, GHCi has has reprofiled these types as generics. The only restriction upon these types is denoted by `Num a =>`, which means that all of these variables must be of the `Num` typeclass (more on these later). This means that all types that could be considered numbers and obey certain laws that the Haskell compiler places upon Nums are admissible in both the arguments and ouput. We can go ahead and use this information to amend our previous function defintion:</p>

<code>
  -- area.hs
  getArea :: Num a => a -> a -> a
  getArea width height = width * height
</code>

<p>Let's load this and make sure it works:</p>
<code>
  > :l area.hs
  > getArea 2.0 4
  >>> 20.0
</code>

<p>Awesome. There's plenty more to be said for this example, but we'll leave it as is for the time being. There's plenty more to be said about Haskell's type system, much of which we'll cover down the line. Truth be told, the system is so omnipresent and powerful that a lot of beginner Haskellians spend much of their time struggling within its confines. More will be said about this type system, but for now, trust me when I say that it is worth taking the time to master this system and really understand how your types are functioning.</p>

<h2>Control Flow</h2>
<p>Haskell's take on control flow far outstrips what we're used to seeing with the standard Algol-family *if-then-else* constructs. Haskell provides us with a number of takes on control flow that encompass function overloading, case statements, and the standard *if-then* constructs that we're used to. Let's jump in with pattern matching.</p>

<h3>Pattern Matching</h3>
<p>Haskell's pattern matching capabilities may be most analogous for most folks to Java's function overloading capabilities, but in reality are far more similar to the list deconstruction capabilities found in Lisp (i.e. Common Lisp, Racket, Scheme, Clojure, etc.). Let's take a look by defining a greeting function which properly handles folks we know, dislike, and don't know:</p>

<code>
  -- greeting.hs
  greeting :: String -> String
  greeting "" = "Hey Henry, let's go drinking today!"
  greeting "Hazel" = "Hey Hazel, are we actually working on OS today?"
  greeting "Laura" = "Hey Laura, we should just probably start that project today!"
  greeting _ = "I don't know you bro."
</code>

<code>
  > :l greeting.hs
  > greeting "Henry"
  >>> "Hey Henry, let's go drinking today!"
  > greeting "Shaan"
  >>> "I don't know you bro."
</code>

<p>As you can see, we're creating instances of our function that have different behaviors upon explicitly defined input patterns. Truth be told, there's no need to be *this* explicit about our inputs: we'll see how we can have more generalizable pattern matching once we start working with lists. One thing that should be noted is the last case. We utilize the `_` character to denote a general wildcard pattern. This takes the place of an `otherwise`, or `else` pattern.</p>

<p>We can use this in a slightly smarter way. Let's say we wanted to pattern-match for individuals whose names start with an 'S', simply so we can ostracize them. We can do this using a clever pattern-matching construct:</p>

<code>
  -- ostracize.hs
  ostracize :: String -> String
  ostracize ('S':_) = "Only fools have a name that starts with S!"
  ostracize ('J':_) = "Ha! John? Jacob? Jingleheimer? More like Jerk!"
  ostracize _ = "I don't have time for you bro."
</code>

<h3>Guards</h3>
Pattern matching is well and good, but oftentimes we want to have conditionals more conveniently nested in our function, rather than the top-level routing that pattern matching provides. For example, let's take a function that tells me whether or not I should stop drinking
<code>
  keepDrinking :: Int -> String
  keepDrinking numDrinks
    | numDrinks < 3 = "Keep on chugging"
    | numDrinks < 6 = "You should probably slow down"
    | numDrinks < 9 = "You should probably stop"
    | otherwise     = "You should probably call an ambulance"
</code>

<p>Here, we're creating more explicit conditional statements that map to program outputs. Rather than the pattern matching, we're able to create conditional statements that allow for true/false evaluation within...</p>

<h3>List Comprehensions</h3>
<p>List comprehensions are one of the most efficient ways we have available to us of constructing lists. If you're a serious Pythonista, you've probably seen these once or twice, but Haskell's take on them is much closer to the way that set construction is dealt with in math:</p>
<code>
  > [x*2 | x <- [1..20]]
  >>>[2,4,6,8,10,12,14,16,18,20]
</code>

<p>As you can see, this takes each element within the range `[1..10]`, doubles it, and adds it to the list. We can spice this up by adding conditional predicates to this:</p>
<code>
  > [x*2 | x <- [1..20], x*2 >= 12]
  >>>[12,14,16,18,20]
</code>

<p>We can further spice this up by having multiple predicates:</p>
<code>
  > [x | x <- [10..20], x /= 13, x /= 15, x /= 19]
  >>>[10,11,12,14,16,17,18,20]
</code>

<p>And we can also perform comprehensions over several lists:</p>

<code>
  > [ x*y | x <- [2,5,10], y <- [8,10,11]]
  >>> [16, 20,22,40,50,55,80,100,110]
</code>

<h2>Lists</h2>
<p>Haskell's take on lists is quite a bit different than what we're used to in imperative languages. Firstly, access time is not constant, but linear. Additionally, lists are constructed in a way that iterative traversal is impossible, forcing us to rely on recursion. Let's take a look at some of these details:</p>
<code>
  > let lostNumbers = [4,8,15,16,23,42]
  > lostNumbers
  >>> [4,8,15,16,23,42]
</code>

<p>Awesome. Let's now construct this list in a completely different way, using the const `:` operator:</p>
<code>
  > let newNumbers = (4:(8:(15:(16:(23:(42:[]))))))
  > newNumbers
  >>> [4,8,15,16,23,42]
</code>

<p>As we can see here, lists are really successive applications of a binary, left-associative appending operator, thus suggesting that much of our list traversal is going to involve peeling elements from the beginning of a list, rather than the iterative type of traversal.</p>

<h2>Local Bindings</h2>
<p>While the Haskell way is to prevent data from being assigned to temporary placeholders, sometimes we do need to alias certain data points to keep our code clean. Take the following example:</p>
<code>
  -- bmi.hs
  bmiTell :: (RealFloat a) => a -> a -> String
  bmiTell weight height
    | weight / height ^ 2 <= 18.5  = "You're underweight!"
    | weight / height ^ 2 <= 25.0  = "You're supposedly normal."
    | weight / height ^ 2 <= 30.0  = "You're fat!"
    | otherwise                    = "You're a whale!"
</code>

<p>We can see that we repeat the `weight / height ^ 2` clause three separate times, so let's see if we can't alias that away:</p>

<code>
  -- bmi.hs
  bmiTell :: (RealFloat a) => a -> a -> String
  bmiTell weight height
    | bmi <= 18.5  = "You're underweight!"
    | bmi <= 25.0  = "You're supposedly normal."
    | bmi <= 30.0  = "You're fat!"
    | otherwise    = "You're a whale!"
    where bmi = weight / height ^ 2
</code>

<p>Awesome, that simplifies our code hugely. Now let's further alias the numerical values here to make a bit more sense of them:</p>

<code>
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
</code>

<p>Awesome, our function is a lot clearer and more readable thanks to our `where` bindings. Now, while this is a </p>

<?php foot(); ?>
