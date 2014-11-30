# The Typeclassopedia
Advanced Haskell is a lot like modern philosophy. To the uninitiated (which includes most of us), reading it is baffling to no end, and there is little to no financial incentive for putting in the hours/days/weeks necessary to gain some insight. As such, serious practitioners are few and far between, and more likely to be found sitting in dark and dusty offices in various computer science deparments rather than the sensually lit workstations of Silicon Valley. Though I do not expect to convince you that this should be otherwise in one day, I do hope to provide insight into the utility of a Haskellian background as the weeks go on.

This lecture is, like most Haskell lectures, an unusual one, and leans heavily upon a foundational text in the Haskell community. The *Typeclassopedia* is a 30-odd page document written by a PhD student at UPenn, Brent Yorgey. While Mr. Yorgey seems to share my own inability to finish school, his document has spread far and wide, primarily because it adddresses the fundamental question at the heart of all advanced Haskell: "What the heck is going on?" Today we'll be going through many of these types and understanding how and why they can be used.
 
## Algebraic Data Types
Before we get into any of the nastiness involving types, we first need to go ahead and understand how we can utilize and extend Haskell's typesystem for our own needs. Haskell allows us to easily create our own types, which we can utilize to fantastic results. Take, for example, the days of the week. In most languages, we would have to resort to some unnatural construct, such as integral indexing, in order to create an effective abstraction for representing weekdays. In Haskell, we can do something far more natural:
```haskell
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
```
Now, we can use this construct for creating some reasonably effective code for handling days of the week:
```haskell
toDo :: Day -> String
toDo Saturday = "Sleep all day!"
toDo Sunday   = "Time to prepare for the week to begin again!"
toDo _        = "Work all day long!"
```
Now, while this is a more readable and (arguably) effective abstraction than arbitrarily assigning weekdays to integer indexes, we can go quite a bit further than this. We'll continue this example a bit when we get into the mechanics of typeclasses. For now, let's continue typed quest by exploring a bit of generic programming in Haskell. Generic programming is quite tricky in most statically type languages, requiring an entire book for explaining its use in Java and at least 12 for C++. Let's create a generic tree in Haskell:
```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)
```
We've just created a type-agnostic, generic binary tree structure. This is a huge win, allowing us to reimagine many of our classic data structures as unbound by many of the classic generic programming concerns. For contrast, let's play around with C++ and Java. Firstly, let's consider a C++ method for summing an array of numbers:
```c++
double sum(double array[], int n)
{
  double result = 0;
  for (int i = 0; i < n; ++i)
    result = result + array[i];
  return result;
}
```
defining trees 
## Typeclasses
similar types == similar vocabularies
allow us to capture vocaburlaries

## Why Haskell?
With most Computer Science courses, the reason for taking it is generally fairly obvious: Why take Databases, Networks, Operating Systems, or Parallel Programming? Because these are the fundamental building blocks of systems programming. Why take Discrete Math and Algorithms? Because you need to know the fundamentals of reasoning about program runtime and performance. Why take Complexity? Because you need to graduate. But with a topic this seemingly esoteric, it is probably worthwhile to take a moment and think about why we're doing this.

Initially, our motivation for teaching this course was to be able to explore more advanced topics in Haskell and further cultivate the Haskell community at the university. This is all well and good, but it only goes so far in convincing folks that this is a good idea.

Haskell is a pretty 
