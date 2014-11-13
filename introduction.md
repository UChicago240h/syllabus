# Lecture 1
## Haskell
As all Haskell courses begin, so does this one: with the most banal description of the language possible. **Haskell is a lazy pure functional language.** What this means, I'm assuming most of you know already: if you don't, this course is probably not for you. At the highest level, this means that Haskell is very, very, very different from most programming languages that you have worked with previously. We'll get into the details of the weirdness in a bit, with much of it hopefully being familiar.

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
As we can see here, we've generated a function that goes about taking in two variables and outputting the product, masquerading as a distance-calculating function. The interesting part here the line `:t distanceTraveled`. This tells GHCi to output the type of the given function. We've given the interpreter limited information about the types of the initial inputs, as we can see from the type signature `a -> a -> a`. As expected from a basic type system, this tells that we have a function taking in two variables, both of some generic type 'a', and outputs a data value of type 'a'. The interesting part is the type binding `Num a =>`

## Immutable State

## Why Haskell?
With most Computer Science courses, the reason for taking it is generally fairly obvious: Why take Databases, Networks, Operating Systems, or Parallel Programming? Because these are the fundamental building blocks of systems programming. Why take Discrete Math and Algorithms? Because you need to know the fundamentals of reasoning about program runtime and performance. Why take Complexity? Because you need to graduate. But with a topic this seemingly esoteric, it is probably worthwhile to take a moment and think about why we're doing this.

Initially, our motivation for teaching this course was to be able to explore more advanced topics in Haskell and further cultivate the Haskell community at the university. This is all well and good, but it only goes so far in convincing folks that this is a good idea.

Haskell is a pretty 