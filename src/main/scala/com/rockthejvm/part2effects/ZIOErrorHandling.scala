package com.rockthejvm.part2effects

import zio.*

import java.io.IOException
import java.net.NoRouteToHostException
import scala.util.Try

object ZIOErrorHandling extends ZIOAppDefault{

  // ZIOs can fail
  val aFailedZIO = ZIO.fail("Something went wrong")
  val failedWithThrowable = ZIO.fail(new RuntimeException("Boom!"))
  val failedWithDescription = failedWithThrowable.mapError(_.getMessage)

  // attempt: run an effect that might throw an exception
  val badZIO: ZIO[Any, Nothing, RuntimeFlags] = ZIO.succeed {
    println("Trying something")
    val string: String = null
    string.length
  } // this is bad

  val anAttempt: Task[Int] = ZIO.attempt{
    println("Trying something")
    val string: String = null
    string.length
  }

  // effectfully catch error
  val catchError = anAttempt.catchAll(e => ZIO.succeed(s"Returning a different value because $e"))
  val catchSelectiveErrors = anAttempt.catchSome {
    case e: RuntimeException => ZIO.succeed(s"Ignoring runtime exception: $e")
    case _ => ZIO.succeed("Ignoring everything else")
  }

  // chain effects
  val aBetterAttempt = anAttempt.orElse(ZIO.succeed(56))

  //fold: handle both success and failure
  val handleBoth = anAttempt.fold(ex => s"Somethign bad happened: $ex", value => s"Length of the string was $value")

  // effectful fold: foldZIO
  val handleBoth_v2 = anAttempt.foldZIO(ex => ZIO.succeed(s"Somethign bad happened: $ex"), value => ZIO.succeed(s"Length of the string was $value"))

  /*
  Conversions between Option/Try/Either to ZIO

 */

  val aTryToZIO: ZIO[Any, Throwable, Int] = ZIO.fromTry(Try(42/0))  // can fail with Throwable

  // either -> ZIO
  val anEither: Either[Int, String] = Right("Success")
  val anEitherToZIO: ZIO[Any, Int, String] = ZIO.fromEither(anEither)

  // ZIO -> ZIO with Either as value channel

  val eitherZIO = anAttempt.either
  // reverse
  val anAttempt_v2 = eitherZIO.absolve

  // option -> ZIO
  val anOption: ZIO[Any, Option[Nothing], Int] = ZIO.fromOption(Some(42))


  /*
  Exercise: implement a version of fromTry, fromOption, fromEither, either, absolve
  using fold and foldZIO
  */

//  def fromOptionChris[A](opt: Option[A]): Task[A] =
//    opt.map{
//      case a: A => ZIO.succeed(a)
//      case None => ZIO.fail(None)
//    }
//  end fromOptionChris


  /*
  Errors = failures present in the ZIO type signatures ("Checked" exception)
  Defects = failures that are unrecoverable, unforeseen, NOT present in the ZIO type signature

  ZIO[R,E,A] can finish with Exit[E,A]
  - Success[A] containing A
  - Cause[E]
    - Fail[E]
    - Die(a: throwable) which was unforeseen
  */
  val divisionByZero: UIO[Int] = ZIO.succeed(1/0)

  val failedInt: ZIO[Any, String, Int] = ZIO.fail("I failed!")
  val failureCauseExposed: ZIO[Any, Cause[String], Int] = failedInt.sandbox
  val failureCauseHidden: ZIO[Any, String, Int] = failureCauseExposed.unsandbox
  // fold with cause
  val foldedWithCause: URIO[Any, String] = failedInt.foldCause(
    cause => s"this failed with ${cause.defects}",
    value => s"this succeed with $value"
  )
  val foldedWithCause2: URIO[Any, String] = failedInt.foldCauseZIO(
    cause => ZIO.succeed(s"this failed with ${cause.defects}"),
    value => ZIO.succeed(s"this succeed with $value")
  )

  /*
  Good practice:
  - at a lower level, your "errors" should be treated
  - at a higher level, you should hide "errors" and assume they are unrecoverable

  */

  def callHTTPEndpoint(url: String): ZIO[Any, IOException, String] =
    ZIO.fail(new IOException("no internet, dummy!"))
  end callHTTPEndpoint

  val endpointCallWithDefects: ZIO[Any, Nothing, String] =
    callHTTPEndpoint("rockthejvm.com").orDie  // all error are now defects
  end endpointCallWithDefects

  def callHTTPEndpointWideError(url: String): ZIO[Any, Exception, String] =
    ZIO.fail(new IOException("No interente!!"))

  def callHTTPEndpoint_v2(url: String): ZIO[Any, IOException, String] =
    callHTTPEndpointWideError(url).refineOrDie[IOException]{
      case e: IOException => e
      case _: NoRouteToHostException => new IOException(s"No route to host $url, can't fetch page")
    }

  // reverse: turn defects into the error channel
  val endpointCallWithError = endpointCallWithDefects.unrefine{
    case e => e.getMessage
  }

  /*
  Combine effects with different error
  */
  trait AppError
  case class IndexError(message: String) extends AppError
  case class DbError(message: String) extends AppError
  val callApi: ZIO[Any, IndexError, String] = ZIO.succeed("page: <html></html>")
  val queryDb: ZIO[Any, DbError, Int] = ZIO.succeed(1)
  val combined: ZIO[Any, IndexError | DbError, (String, RuntimeFlags)] = for {
    page <- callApi
    rowsAffected <- queryDb
  } yield (page, rowsAffected) // lost type safety

  /*
  Solutions:
    - design an error model
    - use Scala 3 union types ZIO[Any, IndexError | DbError, (String, RuntimeFlags)]
    - .mapError to some common error type
  */


  /*
  Exercises
  */
  // 1 - make this effect fail with a TYPED error
  val aBadFailure: ZIO[Any, RuntimeException, RuntimeFlags] = ZIO.succeed[Int](throw new RuntimeException("this is bad"))

  val surfaceError: ZIO[Any, RuntimeException, RuntimeFlags] = aBadFailure.unrefine{ // surfaces out the exception in the error channel
    case e: RuntimeException => e
  }

  val surfaceError2 = aBadFailure.sandbox // exposes the defect in the Cause


  // unrefine is on defects to surface them out, refine is on the error channel

  // 2 - transform a zio into another zio with a narrower exception type
  def ioException[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] =
    zio.refineOrDie{
      case x: IOException => x
    }
  end ioException



  // 3
//  def left[R,E,A,B](zio: ZIO[R, E, Either[A,B]]): ZIO[R, Either[E,A], B] =
//    zio.map {
//      case Left(e) => ZIO.fail(e)
//      case Right(value) => ZIO.succeed(value)
//    }
//

  def left[RE,E,C,B](zio: ZIO[RE, E, Either[C,B]]): ZIO[RE, Either[E, C], B] =
  zio.foldZIO(
    e => ZIO.fail(Left(e)),
    either => either match {
      case Left(a) => ZIO.fail(Right(a))
      case Right(b) => ZIO.succeed(b)
    }
  )
  end left


  // 4
  val database = Map(
    "daniel" -> 123,
    "alice" -> 789
  )

  case class QueryError(reason: String)
  case class UserProfile(name: String, phone: Int)

  def lookUpProfile(userId: String): ZIO[Any, QueryError, Option[UserProfile]] =
    if(userId != userId.toLowerCase())
      ZIO.fail(QueryError("user ID format is invalid"))
    else
      ZIO.succeed( database.get(userId).map(phone => UserProfile(userId,phone)))
  end lookUpProfile

  // surface out all the failed cases of this API
  def betterLookupProfile(userId: String): ZIO[Any, Option[QueryError], UserProfile] =
    lookUpProfile(userId).foldZIO(e => ZIO.fail(Some(e)), {
      case None => ZIO.fail(None)
      case Some(x) => ZIO.succeed(x)
    })
  end betterLookupProfile

  def betterLookuProfile_v2(userId: String): ZIO[Any, Option[QueryError], UserProfile] =
    lookUpProfile(userId).some


  override def run = ???

}



















