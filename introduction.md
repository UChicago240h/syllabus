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
Awesome. There's plenty more to be said for this example, but we'll leave it as is for the time being. Types are something that beginning Haskellians spend a lot of time initially struggling with, so do invest the time in understanding and learning how to compose them.

## Typeclasses

## Pattern Matching

## List Comprehensions

## Guards

## Cases

## If/Then

## Where/Let Clauses

## Recursion

## Zip

## Map

## Fold


## Why Haskell?
With most Computer Science courses, the reason for taking it is generally fairly obvious: Why take Databases, Networks, Operating Systems, or Parallel Programming? Because these are the fundamental building blocks of systems programming. Why take Discrete Math and Algorithms? Because you need to know the fundamentals of reasoning about program runtime and performance. Why take Complexity? Because you need to graduate. But with a topic this seemingly esoteric, it is probably worthwhile to take a moment and think about why we're doing this.

Initially, our motivation for teaching this course was to be able to explore more advanced topics in Haskell and further cultivate the Haskell community at the university. This is all well and good, but it only goes so far in convincing folks that this is a good idea.

Haskell is a pretty 