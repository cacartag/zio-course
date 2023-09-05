package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.*

import java.io.{File, FileWriter}
object Fibers extends ZIOAppDefault {

  val meaningOfLife = ZIO.succeed(42)
  val favLang = ZIO.succeed("Scala")

  // Fiber = lightweight thread
  def createFiber: Fiber[Throwable, String] = ??? // impossible to create manually

  def combinator = for {
    mol <- meaningOfLife.debugThread
    lang <- favLang.debugThread
  } yield (mol, lang)

  val differentThreadIO = for {
    _ <- meaningOfLife.debugThread.fork
    _ <- favLang.debugThread.fork
  } yield ()

  val meaningOfLifeFiber : ZIO[Any, Nothing, Fiber[Throwable, Int]] = meaningOfLife.fork

  // join a fiber
  def runOnAnotherThread[R, E, A](zio: ZIO[R,E,A]) = for {
    fib <- zio.fork
    result <- fib.join
  } yield result

  // awaiting a fiber
  def runOnAnotherThread_v2[R,E,A](zio: ZIO[R,E,A])= for {
    fib <- zio.fork
    result <- fib.await
  } yield result match{
    case Exit.Success(value) => s"succeeded with $value"
    case Exit.Failure(cause) => s"failed with $cause"
  }

  // poll - peek at the result of the fiber RIGHT NOW, without blocking
  val peekFiber = for {
    fib <- ZIO.attempt {
      Thread.sleep(1000)
      42
    }.fork
    result <- fib.poll
  } yield result

  // compose fibers
  // zip
  val zippedFiber = for {
    fib1 <- ZIO.succeed("Result from fiber 1").debugThread.fork
    fib2 <- ZIO.succeed("Result from fiber 2").debugThread.fork
    fiber = fib1.zip(fib2)
    tuple <- fiber.join
  } yield tuple

  // orElse
  val chainedFibers = for {
    fiber1 <- ZIO.fail("not good!").debugThread.fork
    fiber2 <- ZIO.succeed("Rock the JVM").debugThread.fork
    fiber = fiber1.orElse(fiber2)
    messagem <- fiber.join
  } yield messagem

  /*
  Exercises

   */
  // 1 - zip two fibers without using the zip combinator
  // hint: create a fiber that waits for both
  def zipFibers[E,A,B](fiber1: Fiber[E,A], fiber2: Fiber[E,B]):
  ZIO[Any, Nothing, Fiber.Runtime[E, (A, B)]] = {
    val finalEffect = for {
      v1 <- fiber1.join
      v2 <- fiber2.join
    } yield (v1,v2)
    finalEffect.fork
  }

  val zippedFiber_v2 =
    for {
      fib1 <- ZIO.succeed("Result from fiber 1").debugThread.fork
      fib2 <- ZIO.succeed("Result from fiber 2").debugThread.fork
      fiber <- zipFibers(fib1,fib2)
      tuple <- fiber.join
    } yield tuple

  def zipFibersGeneral[E, E1 <: E, E2 <: E, A, B](fiber1: Fiber[E1, A], fiber2: Fiber[E2, B]):
  // Same implemenation
  ZIO[Any, Nothing, Fiber.Runtime[E, (A, B)]] = {
    val finalEffect = for {
      v1 <- fiber1.join
      v2 <- fiber2.join
    } yield (v1, v2)
    finalEffect.fork
  }


  // 2 - same thing with orElse
  def chainFiber[E, A](fiber1: Fiber[E,A], fiber2: Fiber[E,A]): ZIO[Any, Nothing, Fiber.Runtime[E,A]] =
    fiber1.join.orElse(fiber2.join).fork
//      val finalFiber = fiber1.join.map {
//        case Exit.Success(value) => ZIO.succded(value)
//        case Exit.Failure => fiber2.join.map{
//          case Exit.Success(value) => ZIO.succeed(value)
//          case Exit.Failure(e) => ZIO.failure(e)
//        }
//      }
//      finalFiber.fork



  // 3 distributing a task in between many fibers
  // spawn n fibers, count the n of words in each file,
  // then aggregate all the results in one big number
  def generateRandomFile(path: String): Unit = {
    val random = scala.util.Random
    val chars = 'a' to 'z'
    val nWords = random.nextInt(2000) // at most 2000 words
    val content = (1 to nWords)
      .map(_ => (1 to random.nextInt(10)).map(_ => chars(random.nextInt(26))).mkString) // one word for every 1 to nWords
      .mkString(" ")

    val writer = new FileWriter((new File(path)))
    writer.write(content)
    writer.flush()
    writer.close()
  }

  def readAndCount(fileName: String): UIO[Int] = {
    ZIO.succeed {
      val source = scala.io.Source.fromFile(fileName)
      val fromFile = try source.mkString finally source.close()
      fromFile.split(" ").length
    }
  }

  def distributeAcross(n: Int): ZIO[Any, Nothing, RuntimeFlags] =

    val effects = (for {
      i <- 1 to n
      name = s"src/main/resources/testfile_$i.txt"
      zio = readAndCount(name)
      fib = zio.fork
    } yield fib)
      .map(fibEffect => fibEffect.flatMap(_.join))

    effects.reduce{(zioa, ziob) =>
      for {
        a <- zioa
        b <- ziob
      } yield a + b
    }


  end distributeAcross



  override def run = {

    distributeAcross(10).debugThread
  }

    //ZIO.succeed((1 to 10).foreach(i => generateRandomFile(s"src/main/resources/testfile_$i.txt")))
}
























