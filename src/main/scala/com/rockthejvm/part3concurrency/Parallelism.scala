package com.rockthejvm.part3concurrency

import zio._

object Parallelism extends ZIOAppDefault {

  val meaningOfLife = ZIO.succeed(42)
  val favLang = ZIO.succeed("Scala")
  val combined = meaningOfLife.zip(favLang)  // combines/ zips in a sequential

  // combine in parallel
  val combinePar = meaningOfLife.zipPar(favLang)  // combination is parallel

  /*
  - start each on fibers
  - what if one fails? the other should be interrupted
  - what if one is interrupted? the entire thing should be interrupted
  - what if the whole thing is interrupted? need to interrupt both effects
   */

  // try a zipPar combinator
  // hint: fork/join/await, interrupt
  def myZipPar[R,E,A,B](zioa: ZIO[R,E,A], ziob: ZIO[R,E,B]): ZIO[R,E,(A,B)] = {
    val exits = for {
      fiba <- zioa.fork
      fibb <- ziob.fork
      exita <- fiba.await
      exitb <- exita match {
          case Exit.Success(value) => fibb.await
          case Exit.Failure(cause) => fibb.interrupt
      }

    } yield (exita, exitb)

    exits.flatMap {
      case (Exit.Success(a), Exit.Success(b)) => ZIO.succeed((a,b))  // happy path
      case (Exit.Success(_), Exit.Failure(cause)) => ZIO.failCause(cause) // one of them failed
      case (Exit.Failure(cause), Exit.Success(_)) => ZIO.failCause(cause)  // one of them failed
      case (Exit.Failure(c1), Exit.Failure(c2)) => ZIO.failCause(c1 && c2)
    }
  }


  def run = ???


}
