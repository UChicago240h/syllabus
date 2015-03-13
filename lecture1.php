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

<?php foot(); ?>
