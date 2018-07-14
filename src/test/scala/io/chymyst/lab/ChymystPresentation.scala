package io.chymyst.lab

import io.chymyst.jc._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class ChymystPresentation extends FlatSpec with Matchers {

  behavior of "A fully declarative DSL for concurrent computations"

  /*

  What would be a good DSL for expressing concurrent and parallel computations
  in a natural and declarative manner? Can we just say something like "this
  computation needs to be concurrent" and achieve safe, automatically parallel code?

  We will now attempt to _derive_ such a DSL by systematic reasoning from scratch.
  We will then test the use of this DSL as implemented by the `Chymyst` library.

  Example 1: We would like to compute (1 + 2) * (3 + 4) in parallel.
   */

  it should "compute (1 + 2) * (3 + 4) in parallel" in {
    // Define ordinary, sequential functions for this.
    def plus(inp: (Int, Int)): Int = inp._1 + inp._2

    def times(inp: (Int, Int)): Int = inp._1 * inp._2

    // We want to run `plus(1, 2)` and `plus(3, 4)` in parallel,
    // wait for the results, then apply `times()` to the two result values.

    // Here we just declare the input data to "be concurrent".
    val data1: Future[(Int, Int)] = Future((1, 2))
    val data2: Future[(Int, Int)] = Future((3, 4))

    // All computations on concurrent data are then automatically concurrent and
    // run in parallel.
    val resultA: Future[Int] = (data1.map(plus) zip data2.map(plus)).map(times)

    // We could also write this computation using a for/yield block for `Future`s.
    val resultB: Future[Int] = for {
      x1 ← data1
      x2 ← data2
    } yield times((plus(x1), plus(x2)))

    Await.result(resultA, Duration.Inf) shouldEqual 21
    Await.result(resultB, Duration.Inf) shouldEqual 21
  }

  /*
  "Concurrent data" seems to be a productive idea: we just declare some data value
  to "be concurrent", and all computations on it will automatically run in parallel.

  Declaring a data item to be concurrent was done using Scala's `Future`.

  However, a `Future`-valued expression can express only a limited subclass of
  concurrent computations.

  For example, with a for/yield block on a `Future` we cannot:

  - start a new process concurrently every time a new data item arrives;
  - select the concurrent computation that completes first, out of several;
  - decide whether to start a new concurrent computation non-deterministically,
      depending on the current status of other computations now running in parallel.

  Instead of adding _ad hoc_ "features" to `Future`, let us find better primitives.

  How to start a new process concurrently when a new data item "arrives"?

  We need to say that a new concurrent value "became available" for computations.

  It would be good if we could declare a function to "be concurrent", so that
  it will get evaluated automatically whenever its input values become available.

  For example, we would like to define the function `plus()` to "be concurrent",
  and then we want `plus((1, 2))` to run automatically whenever the tuple `(1, 2)`
  "becomes available".

  When `plus((1, 2))` completes, we would naturally expect that its result, `3`,
  will also "become available" in the same sense for any further computations.

  There are some "reactive systems" that implement similar functionality (for example, https://github.com/lihaoyi/scala.rx and https://github.com/ReactiveX/RxScala for Scala).

  However, functional reactive programming is equivalent to a stream graph,
  while we are here interested in a more general approach to concurrency.

  In particular, we would like to be able to solve arbitrary concurrency problems,
  including those that cannot be expressed as an asynchronous stream graph.

  So let us continue reasoning about "concurrent data" and "concurrent functions".

  We would like to rewrite Example 1 like this:

  - declare `plus()` and `times()` as "concurrent functions"
  - make the tuples (1, 2) and (3, 4) available as "concurrent data"
  - somehow say that these are actually inputs for two parallel runs of `plus`
  - somehow say that `times` should get its input from the two results of `plus`

  Imagine we could write (in a somehow modified Scala):

  def_concur plus(inp: (Int, Int)) = ... // Defines `plus()` as concurrent function.

  def emit(x: Any) = ...  // Makes `x` somehow available as "concurrent data".

  emit((1, 2)) // Make the tuple (1, 2) available as concurrent data.
  emit((3, 4)) // Make the tuple (3, 4) available.

  How can we specify that the tuple (1, 2) is intended as input for `plus`?
  We do not want to write `plus((1, 2))` and `plus((3, 4))` explicitly:
  We already declared `plus()` as a concurrent function, and that should be enough.
  So now `plus` should somehow find and consume its input data automatically.

  It is clear that we have to _label_ the concurrent data in some way, in order
  to express the connection between concurrent data and the declared functions.

  Let us attach the name `inp` as a label for the concurrent data for `plus()`.
  The name `inp` corresponds to the name of the input variable in `plus()`.
  
  Instead of `emit((1, 2))`, we will have to say:

    emit("inp", (1, 2)) // Make (1, 2) available as concurrent data labeled "inp".

  This seems to be workable now. We can implement a runtime system that will start
  a new concurrent run of `plus()` automatically on a new thread, as soon as a new
  data value is emitted with the label "inp".

  Of course, we want to have a better encoding of labels than a `String`, but let
  us postpone that issue. For now, we will assume it is a type `Label`.
  
  Now, how could we specify the source of input data for `times()`?
  First, we need to make sure it is not mixed up with the input for `plus()`.
  When we define `times`, we will use another name for its input value, say `inp2`:

  def_concur times(inp2: (Int, Int)) = ... // Use `inp2` as input name.

  However, the problem remains that we need to specify that `inp2` is made up from
  the two results obtained after two parallel runs of `plus()`.

  Let us consider the logical consequences of having "labeled data" as input.
  A "concurrent function" consumes this labeled data and must emit its result value
  also as labeled data! But what should be the label on the result value?

  The label on the result value must be known in the function body, otherwise we
  cannot write the code for the concurrent function.

  In our case, there are two different instances of `plus()`, so their return
  values must carry two different labels.
  But we would like to have only one declaration of `plus()`.
  So, the declaration of `plus()` cannot know the output labels.
  Therefore, the labels must be passed to `plus()` as part of its input value,
  which must thus become a triple `(Label, Int, Int)`.

  Let us then redefine the concurrent functions `plus()` and `times()` like this:

  def_concur plus(inp: (Label, Int, Int)) = {
    val (output_label, x, y) = inp
    emit(output_label, x + y)
  }

  def_concur times(inp1: Int, inp2: Int) = println(inp1 * inp2)

  emit("inp", ("inp1", 1, 2))
  emit("inp", ("inp2", 3, 4))

  Now, we expect this "concurrent program" to work by first executing two parallel
  runs of `plus()` with the given input data items. The result values of these
  two computations are emitted with labels "inp1" and "inp2" respectively. When
  both of these values become available (i.e. are emitted), the computation `times`
  is run and prints the result.

  Here is what we have achieved with this DSL design so far:

  - we can declare functions to be "concurrent"
  - we can declare data to be "concurrent", if we provide a suitable "label"
  - concurrent functions will automatically find and consume their inputs
  - concurrency and parallelism are automatic and "data-driven":
  - if multiple input data is available, multiple runs of concurrent functions
      are automatically started in parallel
  - we do not need to write any code that starts parallel computations explicitly
  - the runtime system can take care of scheduling and create threads as needed

  Let us now simplify the DSL somewhat and strengthen the type usage.

  - Make the "labels" into first-class values, e.g. `inp1`, `inp2` etc.

  - A "label" for a concurrent data item should know about the item's type.
  Say, a label is a value of type `M[A]`, where `M` is a class parameterized by
    the type `A` of the data item. (The `Chymyst` library defines this class.)
  A label of type `M[Int]` must be used for data items of type `Int`, for example.

  - To define labels, we use the constructor of the class `M`:
  */
  val inp1 = new M[Int]("input 1") // Names for debugging purposes only.
  val inp2 = new M[Int]("input 2")
  /*
  - To emit concurrent data, write simply `inp1(7)` instead of `emit("inp1", 7)`.
  This is implemented via `M#apply()` in the `Chymyst` library.

  - To define concurrent functions, we use the `go()` method and a special syntax,
  as defined by the `Chymyst` library.

  Instead of `def_concur times(inp1: Int, inp2: Int) = println(x * y)`, write:
  */
  val times = go { case inp1(x) + inp2(y) ⇒ println(x * y) }
  /*
  In this syntax, `inp1(x)` means "the concurrent data value x, labeled by inp1".

  Note that the labels `inp1` and `inp2` have been already defined to carry
  values of type `Int`. Here, we use these labels in a pattern-matching syntax.
  Type checking of pattern variables `x`, `y` is enforced as usual in Scala code.

  - Define the concurrent function `plus` and its input data label, `inp`:
  */
  val inp = new M[(M[Int], Int, Int)]("input for plus")

  val plus = go { case inp((outputLabel, x, y)) ⇒ outputLabel(x + y) }
  /*
  So far, we have defined the labels `inp`, `inp1`, `inp2` and the concurrent
  functions `plus` and `times` as local values. These definitions are merely
  descriptions of the concurrent logic of our program; they do not cause any
  threads to be created or any computations to start.

  Before we can use this program (by emitting some data items with label `inp`),
  we need to tell the runtime system to "activate" our definitions. Once this is
  done, the runtime will begin to monitor input data items that are emitted.

  It is convenient to make the "activation" into a separate step in the DSL.

  The activation method is called `site()` and takes a list of declared functions:
  */
  site(plus, times)
  /*
  The motivation for the name `site` is that we need to define a "holding site" for
  concurrent data, where the emitted data items with labels `inp`, `inp1`, `inp2`
  will gather, waiting to be consumed by concurrent functions we declared.

  Let's check whether this code actually works:
  */
  it should "run the concurrent functions defined using new DSL" in {
    // Emit the required concurrent input data.
    inp((inp1, 1, 2))
    inp((inp2, 3, 4))
    // This test should print 21 when run.
  }

  /*
  Example 2: A computation that processes all arriving data items concurrently.

  Suppose we expect to receive data items carrying `Double` values: x_1, x_2, ...
  Each time, we compute `cos(x_i)` and then update the running max value `max_cos`.
  We would like to compute all `cos(x_i)` in parallel, allowing `max_cos` to be
  updated in arbitrary order.

  We model x_1, x_2, ... as "concurrent data" inputs. We assume that they will be
  emitted by some other code.

  The input concurrent data needs a label: say, `x_in`. It makes sense to use
  the same label for all x_i, because the computation `cos(x_i)` is the same
  and needs to be run in parallel on each input data item.
  */
  it should "run example 2" in {
    val x_in = new M[Double]("x_i input")

    /* We want to define the computation now, but how?

    val do_cos = go { case x_in(x) ⇒ val result = math.cos(x); ??? }

    The result of the computation needs to be emitted with a label. What label?
    */
    val x_out = new M[Double]("cos output")
    val do_cos = go { case x_in(x) ⇒ x_out(math.cos(x)) }
    /*
    As the `x_in` items arrive, we will consume them and emit `x_out` items.
    Since all input items have the same label `x_in`, the processing will be
    automatically parallelized.

    The output values are emitted asynchronously, as concurrent data with
    the `x_out` label.

    Next, we need to update `max_cos` concurrently and asynchronously. How?

    It seems that `max_cos` itself needs to be declared "concurrent data" as well.
    Let's define a label `max_cos` for it, and emit an initial `max_cos` value.
    */
    val max_cos = new M[Double]("max cos value")
    /*
    Whenever an `x_out` item is emitted, we need to consume it and update `max_cos`.
    For this, clearly, we must define a new concurrent function:

    val update_max = go { case x_out(x) + max_cos(c) ⇒
      val new_max_value = math.max(x, c)
      ???
    }

    We can update the value labeled by `max_cos`, but what should we do with that?
    Likely, we must emit the updated value, `new_max_value`, as concurrent data.
    If we again emit the new value with the old label `max_cos`, we will in effect
    "update" the `max_cos` value. Therefore, the code must be this:

    val update_max = go { case x_out(x) + max_cos(c) ⇒
      val new_max_value = math.max(x, c)
      max_cos(new_max_value)
    }

    Let us write this in a shorter syntax:
    */
    val update_max = go { case x_out(x) + max_cos(c) ⇒ max_cos(math.max(x, c)) }
    /*
    This will work; we are finished with the main code. Now, at any time,
    the concurrent value with label `max_cos` can be updated. The code is concise
    (5 lines) and obviously has no deadlocks or race conditions.

    It would be useful if we could read the current value of `max_cos` at any time.

    However, `max_cos` values are concurrent data, and cannot be directly accessed.
    The only way to access concurrent data is within a concurrent function's body.

    Therefore, we need to define another concurrent function for the purpose of
    reading the values of `max_cos`.

    val read_max = go { case max_cos(c) ⇒ ??? }

    If written like this, the concurrent function will be run whenever `max_cos`
    is emitted. This is clearly not what we want; we need to run that function
    only when we actually need to read the `max_cos` value.

    How can we signal that we want to read that value? The only way to "signal"
    anything is to emit an item of concurrent data. Therefore, we need to define
    another label, say `read`, that will be an additional input of the function.

    val read = new M[Unit]("read max_cos")

    We can now declare a concurrent function that has access to max_cos's value.

    val read_max = go { case max_cos(c) + read(_) ⇒ println(c) }

    Rather than do a `println(c)` here, let us implement a more useful test.
    Let us create an interface between the new concurrent DSL and Scala `Future`s.

    We can create a fresh Scala `Promise` and resolve it inside the function body.
    The `Promise` value will be passed as concurrent data with the `read` label:
     */
    val read = new M[Promise[Double]]("read max_cos")
    val read_max = go { case max_cos(c) + read(p) ⇒ p.success(c) }
    /*
    Now we can emit read() with a fresh `Promise` value, and wait for the result.

    Before testing this code, we need to activate the "concurrent site".
    The site will hold all concurrent data used as input for any concurrent
    functions we defined.
     */
    site(do_cos, update_max, read_max)
    // We can begin the test now.
    // Emit a concurrent value -1.0 with label `max_cos`.
    max_cos(-1.0) // Should be safe, since `math.cos()` is never below -1.
    // Emit some x_in values.
    (1 to 10).foreach(x ⇒ x_in(x.toDouble))
    // Wait for computations to finish. Brittle! But this is just a crude test.
    Thread.sleep(500)
    // Make a fresh `Promise` and emit it as concurrent data with label `read`.
    val pr = Promise[Double]()
    read(pr)
    // Now we can wait for the `Future`.
    val result = Await.result(pr.future, Duration.Inf)
    result shouldEqual 0.96 +- 0.001
  }

  /*
  The Chymyst library documentation uses the "chemical metaphor" to make
  reasoning about the code more visually clear.

  In the "chemical metaphor", the runtime that implements the parallel scheduling of
  concurrent computations is called the "chemical machine". "Concurrent data" is
  called "molecules", and "concurrent functions" are called "reactions".

  Instead of "concurrent data item x with label l", we simply say "molecule l(x)",
  or "molecule l carrying value x". The value `l` is called a "molecule emitter"
  because it is used as a function, e.g. `l(123)`, to emit molecules.

  In the chemical metaphor, reactions "consume" their input molecules.
  The body of a chemical reaction is a function of the values carried on the input
  molecules. A reaction can consume one or more molecules.

  All emitted molecules are held at a reaction site, waiting for reactions to
  begin. If enough molecules are available at a reaction site to start more than
  one copy of a reaction, the chemical machine may start several instances of the reaction on parallel threads. This automatic parallelization is completely safe
  because each instance of the reaction will consume a separate set of input data.

  We successfully implemented some very simple concurrent programs in the new DSL.
  The DSL is high-level, declarative, and expresses purely functional computation
  with no shared memory. All data is passed to functions as labeled "message data".
  So, one may say that this DSL implements a kind of "message-passing concurrency".

  In the academic literature, this concurrency paradigm is called "join calculus".
  But I do not advise you to read any academic literature on this subject, because
  it is not written with programmers in mind, and does not explain clearly how this
  paradigm can be used in practice for writing concurrent and parallel code. I am
  writing new tutorial-level documentation to explain chemical machine programming.

  My experience with the chemical machine shows that, after some practice,
  one can systematically derive safe, declarative, and very concise implementations
  for a large number of concurrency algorithms I have examined.

  Programming in the chemical machine is "systematic" because there are very few
  primitives and very few choices for the programmer to control the concurrent
  computation. Usually, there is only one way of solving a given concurrency
  problem, and one can quickly arrive at the solution by logical reasoning, as I
  illustrated by the examples in this tutorial.

  The chemical machine itself can be embedded as a library-based DSL in many
  programming languages. There exist implementations for OCaml, Haskell, C#, C++,
  Python, and perhaps other languages.

  Here I have used my Scala implementation of the chemical machine. The source code
  with extensive documentation is at https://github.com/Chymyst/chymyst-core/.

  At the moment, the Scala implementation is limited to a single JVM. In the near
  future, I hope to add features for automatic, declarative distributed programming.

  */
}
