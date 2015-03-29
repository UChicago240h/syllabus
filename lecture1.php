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
<?php foot(); ?>
