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
Haskell's type system is simultaneously its most powerful and most irritating feature. Type safety allows us to write hugely robust programs, but often at the expense of accessibility. If you have not already, you will spend a significant amount of time tearing your hair out over type signatures. Let's take begin previous example, which we'll put in a file called *area.hs* (Note the lack of `let` bindings):
```haskell
getArea :: Int -> Int -> Int
getArea width height = width * height
```
Now let's load this file into GHCi and give it a whirl:
```haskell
> :l area.hs
> getArea 4 5
>>> 20
```
This works as before
## Immutable Variable
```haskell
let rate = 10
let time = 20
let distance = rate * time
distance
```
Much as we can define variables, we can define the core unit of any given functional language, i.e. functions:
```haskell
let distanceTraveled speed time = speed * time
:t distanceTraveled
>> distanceTraveled :: Num a => a -> a -> a
distanceTraveled 20 100
```
As we can see here, we've generated a function that goes about taking in two variables and outputting the product, masquerading as a distance-calculating function. The interesting part here the line `:t distanceTraveled`. This tells GHCi to output the type of the given function. We've given the interpreter limited information about the types of the initial inputs, as we can see from the type signature `a -> a -> a`. As expected from a basic type system, this tells that we have a function taking in two variables, both of some generic type 'a', and outputs a data value of type 'a'. The interesting part is the type binding `Num a =>`. What has happened here is that the Haskell interpreter has detected that the type binding must be able to perform the `*` operation. The interpreter knows that any types conforming to the `*` operation must by part of the `Num` typeclass.

Haskell's type system is extremely powerful and very strict. While in Python, you're free to run willy-nilly with duck-typing, Haskell is a bit more strict when it comes to this. To see this, let's take a look at a degenerate example:
```haskell

```
Let's try to run this bad boy:
```haskell
:l distance.hs
getDistance 8.0 34
```
This causes the compiler to throw a fit, complaining that 8.0 is not an instance of `Int`, as it epxects. We can quite easily fix this by killing the type signature and allowing the compiler to generate a generic type signature for us:
```haskell
getDistance speed time = speed * time

:l distance.hs
getDistance 8.0 34
>> 272.0
:t getDistance
>> Num a => a -> a -> a
```
Types are something that beginning Haskellians spend a lot of time initially struggling with. Learn them, learn them well.

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