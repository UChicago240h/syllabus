<php>
<h1>Typeclassopaedia</h1>
<p>Haskell is has a lot in common with modern philosophy. You're more likely to find its adherents in the halls of academia than you are to find them in the wild. The ideas you will come upon while studying Haskell are incomprehensible to most and difficult to master, but they are hugely influential, and becoming more so with the rise of JVM-based functional languages, primarily Scala and Clojure.</p>
<p>Today, we're going to focus on one of the foundational texts in Haskell literature: the Typeclassopaedia. The Typeclassopedia was initially written as a paper by Brent Yorgey, a graduate student at UPenn. The text, and today's lecture, is dedicated to explaining Haskell's core type classes. These type classes provide much of Haskell's power and expressiveness, and mastering them is essential to mastering Haskell. This stuff is confusing, it is complicated, and it is the core of what makes Haskell such a next-level language.</p>

<h1>Type Class Preliminaries</h1>
<p>Before we can start getting into the meaty details of Haskell's core type classes, we have to first wrap our heads around what a type class is. As we remember from our first encounter with Haskell, the language allows us to define our own data types. For example, let's consider a basic example: let's say we want to define a function that operates on the days of the week. In Python, the core <code>datetime</code> library handles this by binding days to integers, with Sunday defined as 0 and the remaining days being defined as expected. While this is well and good, Haskell allows us to do something far more elegant:</p>
<code>
  data WeekDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
</code>

</php>
