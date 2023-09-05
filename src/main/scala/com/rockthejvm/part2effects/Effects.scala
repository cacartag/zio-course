package com.rockthejvm.part2effects

import scala.concurrent.Future
import scala.io.StdIn.readLine
import scala.util.Try

object Effects {

  // functional programming
  // EXPRESSIONS
  def combine(a: Int, b: Int): Int = a + b

  // local reasoning = type signature describes the kind of computation that will be performed
  // referential transparency = ability to replace an expression with the value that it evaluates to
  val five = combine(2,3)
  val five_2 = 2 + 3
  val five_v3 = 5

  // not all expression are RT
  // example 1: printing
  val resultOfPrinting: Unit = println("Learning ZIO")
  val resultOfPrinting_v2: Unit = () // not the same

  // example 2: changing a variable
  var anInt = 0
  val changingInt: Unit = (anInt = 42) // side effect
  val changingInt_v2: Unit = () // not the same program

  // side effect are inevitable

  /*
  Effect desires
  - the type signature describes what kind of computation in will perform
  - the type signature describe the type of VALUE that it will produce
  - if side effect are required, construction must be separate from the EXECUTION


  /*

  */
  Example: Options = possibly absent values
  - type signature describe the kind of computation = a possibly absent value
  - type signature says that the computation returns an A, if the computation does produce something
  - no side effect are needed

  => Option is an effect
  */

  val anOption: Option[Int] = Option(42)

  /*
  Example 2: Future
  - describes an asynchronous computation
  - produces a value of type A, if it finishes and it's successful
  - side effects are required, construction is NOT SEPARATE from execution

  => Future is NOT an effect
  */
  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future(42)

  /*
  Example 3: MyIO
  - describes a computation that which might perform side effects (including those performing side effects)
  - produces a values of type A if the computation is successful
  - side effects are required, construction IS SEPARATE from execution

  My IO IS AN EFFECT!
  */

  case class MyIO[A](unsafeRun: () => A){
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIOWithSideEffects: MyIO[Int] = MyIO(() => {
    println("producing effects")
    42
  })

  /*
  Exercises - create some IO which
  1. measure the current time of the system
  2. measure the duration of a computation
    - use exercise 1
    - use map/flapMap combination of MyIO
  3. read something from the console
  4. print something to the console (e.g "what's your name"), then read, then print a welcome message
  */

  // 1
  val currentTime: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  // 2
  def measure[A](computation: MyIO[A]): MyIO[(Long, A)] = {
     currentTime
       .flatMap(start =>
       computation.flatMap(done =>
         currentTime.map(endTime =>
           (endTime - start, done)
         )
       ))
  }

  def danielsMeasure[A](computation: MyIO[A]): MyIO[(Long, A)] =
    for{
      startTime <- currentTime
      calculate <- computation
      endTime <- currentTime
    } yield(endTime - startTime, calculate)


  // 3

  val readSomething: MyIO[String] = {
    MyIO(() => readLine())
  }

  // 4

  def readAndWrite: MyIO[String] =
    MyIO(() => println("What is your name?"))
      .flatMap(_ =>
        readSomething.map(y =>
          s"Your name is $y"
        )
      )

/*
* A simplified ZIO
* */

  case class MyZIO[-R, +E, +A](unsafeRun: R => Either[E, A]) {
    def map[B](f: A => B): MyZIO[R,E, B] =
      MyZIO(r => unsafeRun(r) match{
        case Left(e) => Left(e)
        case Right(v) => Right(f(v))
      })

    def flatMap[R1 <: R, E1 >: E, B](f: A => MyZIO[R1, E1,B]): MyZIO[R1, E1, B] =
      MyZIO(r => unsafeRun(r) match {
        case Left(e) => Left(e)
        case Right(v) => f(v).unsafeRun(r)
      })
  }

  def main(args: Array[String]): Unit = {

    def noMatter: Int = {
      Thread.sleep(4000)
      86
    }

    val wrappedNoMatter: MyIO[Int] = MyIO(() => noMatter)

//    println(measure[Int](wrappedNoMatter).unsafeRun())

    println(readAndWrite.unsafeRun())

    //anIOWithSideEffects.unsafeRun()
  }


}













