package io.chymyst.lab

import io.chymyst.jc._

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Random.nextInt
import scala.util.{Failure, Success, Try}

object ChymystGuessGame extends App {
  /*
  Guess-a-number game with asynchronous questions.
  
  The machine chooses a number `x` between 1 and 100.
  The player asks questions of the form "Is the number X greater than N" for N chosen by the player.
  The machine answers "yes" or "no" after an unpredictable delay.
  
  The player should be able to ask questions at any time.
  The answers should arrive whenever they are ready.
  However, the machine may not print any answers while the player is typing a question.
  
  Player starts typing a question by pressing Enter.
  A question consists of an integer value. Player finishes typing a question by pressing Enter.
  */

  /** Print a prompt. Wait for the player to input an integer number on the standard input stream.
    * If the player does not input an integer, repeat the question.
    *
    * @return An integer number given by the player.
    */
  @tailrec
  def readGuess(): Int = {
    print("Your question: Is X greater than ")
    Try(readLine().toInt) match {
      case Failure(_) ⇒
        println(s"Error, try again.")
        readGuess()
      case Success(guess) ⇒
        println("Please wait for the answer.")
        guess
    }
  }

  /** Given a player's guess, prepare the response message.
    *
    * @param x     The secret number chosen by the machine.
    * @param guess The player's guess.
    * @return The machine's response to the player's guess.
    */
  def answerMessage(x: Int, guess: Int): String =
    if (x == guess)
      s"Correct, the number is $x."
    else
      s"The number X is ${if (x > guess) "" else "not "}greater than $guess."

  /** Wait a random number of milliseconds between `delayMs` and `2*delayMs`.
    */
  def delay(): Unit = {
    val delayMs = 10000
    Thread.sleep(delayMs.toLong + nextInt(delayMs))
  }

  // Initialize game: choose a random number `x` between 1 and 100.
  val maxX = 100
  val x = 1 + nextInt(maxX)
  println(s"I have chosen a number between 1 and $maxX. Press Enter to begin.")

  /*
  The console is in one of two states: either the player is in the process of entering a number, or the console is free for the machine to print its answers until the player decides to press Enter in order to begin entering a number.
  Initially, the console is free for the machine.
  Either the machine may print an answer, or the player may begin entering a number.
  This is a contention on a molecule that we may call `canShow()`.
  */
  val canShow = m[Unit]
  /*
  Therefore, we need two reactions that consume `canShow()`: One reaction will print a machine's answer, the other reaction will read the player's question.
  The molecule `canShow()` serves as a mutex contention token for these two reactions.
  We begin by drafting the code of these reactions:
  
  ```scala
  go { case canShow(_) ⇒ val guess = readGuess(); canShow() ... }
  go { case canShow(_) ⇒ println(answer_message); canShow() ... }
  ...
  canShow()

  ```
  
  The machine's answer can be printed only if we have the message string, but `canShow()` is a contention token and does not carry that data.
  Therefore, we need another molecule carrying the message string for the machine's answer. 
   */
  val answer = m[String]
  /*
  Now we can write this reaction:
  
  ```scala
  go { case canShow(_) + answer(msg) ⇒ println(msg); canShow() }
  
  ```
  
  The machine's answer needs to be emitted by a reaction of the form
  `go { case question(...) ⇒ delay(); answer(...) }`.
  To provide input to that reaction, we need a molecule that carries the player's question (an `Int` value).
   */
  val question = m[Int]
  /*
  What reaction will emit `question()` molecules? The reaction that reads the player's input. It should therefore look like this,
   
  ```scala
  go { canShow(_) ⇒ val guess = readGuess(); question(guess); canShow() }
  
  ```
  
  We have not yet implemented the functionality of requiring the player to press Enter in order to start entering a new question.
  A reaction for this needs to have code such as `go { ... ⇒ readline(); ... }`.
  This reaction should be able to start only when the player has finished asking the previous question, so that the console is idle.
  Therefore, its input molecule, say `idle()`, should be emitted by the reaction that reads the question.
  In turn, the reaction that reads the player's question should be unable to start while the console is idle.
  Therefore, we need an additional input molecule, say `canAsk()`, for that reaction. 
  */
  val idle = m[Unit]
  val canAsk = m[Unit]
  /*
  ```scala
  go { canShow(_) + canAsk(_) ⇒
    val guess = readGuess()
    question(guess); canShow(); idle()
  }
  go { idle() ⇒ readline(); canAsk() }
  
  ```
  
  We would like to implement the end-of-game functionality: if the guess is correct, we exit the application.
  */
  val waitForGameOver = b[Unit, Unit]
  val gameOver = m[String]
  /*
  The `gameOver()` molecule will be emitted instead of `answer()` molecule.
  
  The complete code now looks like this:
  */

  site(
    go { case canAsk(_) + canShow(_) ⇒ question(readGuess()); canShow(); idle() },
    go { case idle(_) ⇒ readLine(); canAsk() },
    go { case question(guess) ⇒ 
      delay()
      val ans = answerMessage(x, guess)
      if (guess == x) gameOver(ans) else answer(ans) },
    go { case answer(ans) + canShow(_) ⇒ println(ans); canShow() },
    go { case gameOver(msg) + waitForGameOver(_, r) ⇒ println(msg); r()}
  )

  // Initially, the console is idle.
  idle()
  canShow() // This molecule is a contention token and must be emitted at the start.
  waitForGameOver()
  System.exit(0)

  // To improve performance when many questions are asked, we should inline the `idle() ⇒` reaction body into the previous reaction.
  // There is also another issue with this code:
  // If a single reaction site is used and the player asks many questions quickly, all threads will be busy computing answers, and reactions for `readGuess()` will be greatly delayed.
  // To fix this, we should split the reactions across two separate reaction sites, so that the thread starvation does not affect the reactions handling the console interaction.
}
