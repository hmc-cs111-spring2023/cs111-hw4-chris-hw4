# Practice Scala

The goal of this assignment is to help you become familiar with Scala and the
development tools we'll be using this semester. When you're done, you should:

- feel you've had good practice with the basics of Scala: variables, data
  structures, functions, tests
- have lots of questions about why Scala was designed the way it was

These are the parts of the assignment:

0. Set up your development environment.
1. Write some recursive functions and tests for those functions.
2. Fill in the implementation for a guessing game.
3. Add fluency to an image-processing library.
4. Reflect on your experience programming in Scala.
5. Commit your code, push it to Github, and submit it on Gradescope.
6. Feedback on another assignment.

**Be sure to skim the whole document, before you start coding!**

## Resources

You will need to explore a little bit of Scala on your own. In fact, that's the whole
point of this assignment :). Here are some general resources on learning Scala that might
help:

- [Scala learning overview](http://www.scala-lang.org/documentation/)
- [Scala style guide](http://docs.scala-lang.org/style/)
- [Scala cheatsheet](http://docs.scala-lang.org/cheatsheets/)

Please also ask questions in class, on Discord, and in office hours!

## Part 0: Set up your environment

There are (at least) three ways to work on the assignment:

1. In Codespaces
2. On your own machine, using Docker
3. On your own machine, using your own installation of Scala

The instructions below are specific to Visual Studio Code, though you're of course free to
use another editor!

### Codespaces

Just click on the button at the top of this file. Once the Codespace loads, you should be
good to go!

I recommend that you do not use `sbt` in continuous mode. I think that doing so uses quite
a bit of resources, which may cause things to start crashing.

I also recommend that you **commit and push often**. The codespace is a temporary resource,
and if you don't push to your repo, your changes may not be saved.

### On your own machine, using Docker

If you are familiar and comfortable with Docker, you can clone the repo and open the
folder in VS Code. If you have the "Dev Containers" extension installed, VS Code should
prompt you to reopen the folder in the container. Once it does, you can treat it the same
way you would Codespaces!

Note that this option might not play well with all architectures, so if you have some
troubles and we aren't able to resolve them quickly, one of the other options is best.

### On your own machine, using your own installation of Scala

You can also download and install [Scala](https://www.scala-lang.org/download/)
and [sbt](https://www.scala-sbt.org/download.html) on your own machine.

We are using **Scala version 3.2.2** and **sbt version 1.8.2**.

You can optionally install the Scala (Metals) extension into VSCode, for syntax
highlighting and other, Scala-related tools.

Note that installing software on your machine can sometimes be difficult, so you might try
one of the other two versions if you're not able to make quick progress.

## Part 1: Recursive functions

### `collatz`

In the file `src/main/collatz/Collatz.scala`, write a function named `collatz`, which
takes one positive integer argument. The function should compute the following
mathematical function:

$$
\text{collatz}(n) =  \begin{cases}
n/2 & \text{if } n \text{ is even} \\
3n + 1 & \text{if } n \text{ is odd}
\end{cases}
$$

In the file `src/test/collatz/CollatzSpec.scala`, write some test cases for your `collatz`
function. Here are a few input-output pairs that are good to test:

- $\text{collatz}(6) = 3$
- $\text{collatz}(7) = 22$
- $\text{collatz}(1) = 4$
- $\text{collatz}(2) = 1$
- $\text{collatz}(42) = 21$
- $\text{collatz}(101) = 304$

**Note:** If you want to use VS Code's GUI test runner, I was able to get it working by doing the following:
1. Run `sbt test`.
1. Quite `sbt` and the terminal. (I think `sbt` can't be running at the same time as the GUI tries to run tests).
1. Open the GUI test pane, and try running the tests.

This feature seems to be buggy, though, especially in Codespaces.

### `collatzCount`

In the file `src/main/collatz/Collatz.scala`, write a **recursive** function named `collatzCount`, which
takes one positive integer argument. The function should return the smallest number of times that `collatz`
(your previous function) must be called, when given an input of `n`, to arrive at a value of `1`.

For example, consider the expression `(collatz 3)`. Let's use the symbol `==>` to mean
"evaluates to." Then

`(collatz (collatz (collatz (collatz (collatz (collatz (collatz 3))))))) ==> 1`

so, `(collatz-count 3) ==> 7`, because there are seven calls to `collatz` above.

In the file `src/test/collatz/CollatzSpec.scala`, write some test cases for your `collatzCount`
function. Here are a few input-output pairs that are good to test:

- $\text{collatzCount}(1) = 0$
- $\text{collatzCount}(101) = 25$
- $\text{collatzCount}(1111) = 31$
- $\text{collatzCount}(267) = 21$

## Part 2: Guessing game

In this part, we will complete a simple implemention of the game
["Mastermind"](<https://en.wikipedia.org/wiki/Mastermind_(board_game)>).

Our implementation is text-based. At the start of each game, the application will create a
random _board_. The board is a sequence of four colors, chosen from the set
`{Blue, Yellow, Red, Green}`. The player will then try to guess the board. After each
guess, the application will tell the player how many colors they guessed correctly, and
how many colors they guessed correctly and in the correct position. The game ends when the
player guesses the entire board correctly.

Here is an example run of the game:

```
Enter a guess for spot 1: R
Enter a guess for spot 2: B
Enter a guess for spot 3: Y
Enter a guess for spot 4: G
1 color(s) are in the correct place.
2 color(s) are correct but in the wrong place.

Enter a guess for spot 1: B
Enter a guess for spot 2: R
Enter a guess for spot 3: Y
Enter a guess for spot 4: G
3 color(s) are in the correct place.
0 color(s) are correct but in the wrong place.

Enter a guess for spot 1: G
Enter a guess for spot 2: R
Enter a guess for spot 3: Y
Enter a guess for spot 4: G
2 color(s) are in the correct place.
1 color(s) are correct but in the wrong place.

Enter a guess for spot 1: B
Enter a guess for spot 2: R
Enter a guess for spot 3: B
Enter a guess for spot 4: G
4 color(s) are in the correct place.
0 color(s) are correct but in the wrong place.

Congratulations! You figured out the board was BRBG
```

**Your task:** The file `src/main/mastermind/Mastermind.scala` has a partial
implementation of the game. Complete the undefined functions so that the game works as
described above.

Here are a few notes and suggestions:

- The body of each undefined function has the code `???`. Replace this code with your
  implementation.
- Read over the file, so you understand how the game works.
- The game is not very robust to errors. It expects the user to enter a single, valid
  character for each guess. You do not need to handle invalid input.
- You can run the game in sbt with the command **`runMain mastermind`**.
- If you want to exit out of the game, I recommend pressing Return during a guess.
- There are multiple ways to implement the functions: `if/then/else` and `match` can be
  good options for some of them.
- You will probably need to look up the documentation for random numbers in Scala. You
  can find it [here](https://www.scala-lang.org/api/current/scala/util/Random.html).
- It may help to know that we can make a string from a list of characters using the
  [`.mkString`
  method](https://www.scala-lang.org/api/current/scala/collection/Seq.html#mkString:String).
- Be sure to use functions you have already written, as needed! The total amount of code
  you need to write is probably fewer than 20 lines.
- Your implementation doesn't have to output exactly the same text as the example above.
  The only thing we are interested in is that the game works as described.

## Part 3: Fluency

The file `src/main/images/Image.scala` contains an implementation of a simple image-processing
library. The file `src/main/images/Program.scala` is a program that uses the library.
These files are ports from the non-fluent version we saw in Python. Modify both files so
that we can use the library as described in the comment at the bottom of `Program.scala`.

Note, you can run the program in sbt with the command **`runMain image_program`**. The
output file will be in the `output.png` file at the root of this directory.

Be sure that all the code still compiles and runs after your changes!

## Part 4: Write about Scala

After completing the exercises, take about 20 minutes to collect and write about
your initial impressions of Scala. Write about them in `scala-thoughts.md` Some
possible topics include:

- What's easy to do in Scala? What's not?
- Sor far, what is/are your favorite language design choice(s) that the designers of Scala
  made? Why?
- So far, what is/are your least favorite language design choice(s)? Why? And why do
  you think the designers made that / those choice(s)?
- What Scala features would you like to learn more about?

## Commit, push, and submit to Gradescope

Be sure to remember to commit and push your work to GitHub, and submit on Gradescope! No
matter where you are working (including Codespaces), you will need to push to GitHub, just
as you have for previous assignments.

## Peer-review other people's work

After the submission deadline, I will post critique partners. Each person will add
feedback to someone else's work; though that might mean that some people receive multiple
critiques. You should look through their solutions to each of the parts, as well as their
`scala-thoughts.md` file. You don't need to comment on everything, but find some
interesting parts of their work to evaluate. Some questions you might consider / answer
when providing feedback:

- How does this team's solution compare to yours? To the provided solutions?
  To other people's solutions? Is there anything they did that you like better?
  Is there any way to improve their solution?
- How readable is their code? Is it easy to understand what it does? Is it easy to
  understand how it works?
- How Scala-y is the code? Most of us are just learning the language, and we
  can help each other spot cases when a functional approach would have worked in
  place of an imperative one, etc.
- If a team has raised a question in the comments of their code, or in their
  writings about Scala, try to answer it!
