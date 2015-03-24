<php>
<h1>Typeclassopaedia</h1>
<p>Haskell is has a lot in common with modern philosophy. You're more likely to find its adherents in the halls of academia than you are to find them in the wild. The ideas you will come upon while studying Haskell are incomprehensible to most and difficult to master, but they are hugely influential, and becoming more so with the rise of JVM-based functional languages, primarily Scala and Clojure.</p>
<p>Today, we're going to focus on one of the foundational texts in Haskell literature: the Typeclassopaedia. The Typeclassopedia was initially written as a paper by Brent Yorgey, a graduate student at UPenn. The text, and today's lecture, is dedicated to explaining Haskell's core type classes. These type classes provide much of Haskell's power and expressiveness, and mastering them is essential to mastering Haskell. This stuff is confusing, it is complicated, and it is the core of what makes Haskell such a next-level language.</p>

<h1>Type Class Preliminaries</h1>
<p>Before we can start getting into the meaty details of Haskell's core type classes, we have to first wrap our heads around what a type class is. As we remember from our first encounter with Haskell, the language allows us to define our own data types. For example, let's consider a basic example: let's say we want to define a function that operates on the days of the week. In Python, the core <code>datetime</code> library handles this by binding days to integers, with Sunday defined as 0 and the remaining days being defined as expected. While this is well and good, Haskell allows us to do something far more elegant:</p>
<code>
  data WeekDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
</code>
<p>This is a huge win: it allows us to write code that effectively typechecks and more effectively conveys our intentions. With languages like Python or C, we have to spend some time browsing through documentation before we can understand our code's intention. For example, let's consider a quick C function that let's us know what we're going to be doing each day of the week:</p>
<code>
  def toDo(day):
    if day == 0:
      return "Chilling and preparing for the week to begin!"
    if day == 1:
      return "Beginning the week!"
    if day > 1 and day < 5:
      return "Working all day!"
    if day == 5:
      return "Preparing for the weekend!"
    if day == 6:
      return "Sleeping all day long!"
    return "I don't know what day that is!"
</code>
<p>Now, let's try writing this code in Haskell:</p>
<code>
  data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

  toDo :: Day -> String
  toDo day
    | Sunday = "Chilling and preparing for the week to begin!" 
    | Monday = "Beginning the week!"
    | Friday = "Preparing for the weekend!"
    | Saturday = "Sleeping all day long!"
    | otherwise = "Working all day!"
</code>
<p>Great. We now have a function and datatype that very clearly and concisely communicates the intentions behind our code. There is no need to look up external documentation. Another bonus in this is that it eliminates the need for the clause that returns a failure message in case of an unrecognized input. Since the <code>Day</code> type that we defined is restricted to the members of the enum we have defined, any values outside our constraints will throw a type error during compilation.</p>
<p>Now, this is all well and good, but there are certain issues we have to deal with when creating new data types. Let's illustrate this by trying to do something a bit more than just pure functions with our new data type. We'll first write a function that converts integers to weekdays:</p>
<code>
  intToDay :: Int -> Maybe Day
  intToDay x
    | 0         = Just Sunday
    | 1         = Just Monday
    | 2         = Just Tuesday
    | 3         = Just Wednesday
    | 4         = Just Thursday
    | 5         = Just Friday
    | 6         = Just Saturday
    | otherwise = Nothing
</code>
<p>Now, le's try to make use of this function. Let's write a simple command program that takes an integer from the command line and prints out the corresponding day:</p>
<code>
  import System.Environment (getArgs)

  main :: IO ()
  main = do
    args <- getArgs
    day <- intToDay $ read $ head args
</code>
</php>
