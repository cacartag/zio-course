package com.rockthejvm.part2effects

import zio.*

import java.util.concurrent.TimeUnit

object ZIODependencies extends ZIOAppDefault{

  // app to subscribe users to newsletter
  case class User(name: String, email: String)

  class UserSubscription(emailService: EmailService, userDatabase: UserDatabase) {
    def subscribeUser(user: User): Task[Unit] =
      for {
        _ <- emailService.email(user)
        _ <- userDatabase.insert(user)
      } yield ()
  }

  object UserSubscription{
    def create(emailService: EmailService, userDatabase: UserDatabase) =
      new UserSubscription(emailService, userDatabase)
    end create

    val live: ZLayer[EmailService with UserDatabase, Nothing, UserSubscription] =
      ZLayer.fromFunction(create)

  }
  class EmailService{
    def email(user: User): Task[Unit] =
      ZIO.succeed(println(s"You've just been subscribed to Rock the JVM. Welcome, ${user.name}!"))
  }

  object EmailService{
    def create(): EmailService = new EmailService

    val live: ZLayer[Any, Nothing, EmailService] =
      ZLayer.succeed(create())
  }
  class UserDatabase(connectionPool: ConnectionPool){
    def insert(user: User): Task[Unit] = for{
      conn <- connectionPool.get
      _ <- conn.runQuery(s"insert into subscribers(name, email) values (${user.name}, ${user.email})")
    } yield ()
  }

  object UserDatabase{
    def create(connectionPool: ConnectionPool) =
      new UserDatabase(connectionPool)
    end create

    val live: ZLayer[ConnectionPool, Nothing, UserDatabase] =
      ZLayer.fromFunction(create _)

  }
  class ConnectionPool(nConnections: Int){
    def get: Task[Connection] =
      ZIO.succeed(println("Acquired Connection")) *> ZIO.succeed(Connection())
  }

  object ConnectionPool{
    def create(nConnections: Int) =
      new ConnectionPool(nConnections)
    end create

    def live(nConnections: Int): ZLayer[Any, Nothing, ConnectionPool] =
      ZLayer.succeed(create(nConnections))
  }

  case class Connection() {
    def runQuery(query: String): Task[Unit] =
      ZIO.succeed(println(s"Executing query: $query"))
  }

  val subscriptionService = ZIO.succeed( // Dependency injection
    UserSubscription.create(
      EmailService.create(),
      UserDatabase.create(
        ConnectionPool.create(10)
      )
    )
  )

  /*
  "clean DI" has drawbacks
  - does not scale for many services
  - DI can be 100x worse
    - pass dependencies partially
    - not having all deps in the same place
    - passing dependencies multiple times
   */

  def subscribe(user: User): ZIO[Any, Throwable, Unit] = for {
    sub <- subscriptionService  // service is instantiated at the point of call
    _ <- sub.subscribeUser(user)
  } yield ()

  // risk leaking resources if you subscribe multiple users in the same program
  val program = for {
    _ <- subscribe(User("", ""))
    _ <- subscribe(User("", ""))
  } yield ()

  // alterative
  def subscribe_v2(user: User): ZIO[UserSubscription, Throwable, Unit] = for {
    sub <- ZIO.service[UserSubscription] // ZIO[UserSubscription, Nothing, UserSubscription]
    _ <- sub.subscribeUser(user)
  } yield ()

  val program_v2 = for {
    _ <- subscribe_v2(User("", ""))
    _ <- subscribe_v2(User("", ""))
  } yield ()


  /*
  - we don't need to care about dependencies until the end of the world
  - all ZIOs requiring this dependency will use the same instance
  - can use different instances of the same type for different needs (e.g testing)
  - layers can be created and composed much like regular ZIOs + rich APIs
   */

  /*
  ZLayers
   */

  val connectionPoolLayer: ZLayer[Any, Nothing, ConnectionPool] = ZLayer.succeed(ConnectionPool.create(10))
  // a layer that requires a dependency (higher layer) can be built with ZLayer.fromFunction
  // (and automatically fetch the function arguments and place them into the ZLayer's dependency/environment type argument)
  val databaseLayer: ZLayer[ConnectionPool, Nothing, UserDatabase] = ZLayer.fromFunction(UserDatabase.create)
  val emailServiceLayer: ZLayer[Any, Nothing, EmailService] = ZLayer.succeed(EmailService.create())
  val userSubscriptionServiceLayer: ZLayer[EmailService with UserDatabase, Nothing, UserSubscription] = ZLayer.fromFunction(UserSubscription.create _)

  // composing layers
  // vertical composition
  val databaseLayerFull: ZLayer[Any, Nothing, UserDatabase] = connectionPoolLayer >>> databaseLayer
  // horizontal composition: combines the dependencies of both layers AND the value of both layers
  val subscriptionRequirementsLayer: ZLayer[Any, Nothing, UserDatabase & EmailService] = databaseLayerFull ++ emailServiceLayer
  // mix and match
  val userSubscriptionLayer: ZLayer[Any, Nothing, UserSubscription] = subscriptionRequirementsLayer >>> userSubscriptionServiceLayer


  // best practice: write "factory" methods exposing layers in the companion object of the services
  val runnableProgram: ZIO[Any, Throwable, Unit] = program_v2.provideLayer(userSubscriptionLayer)

  // magic
//  val runnableProgram_v2 = program_v2.provide(
//    UserSubscription.live,
//    EmailService.live,
//    UserDatabase.live,  // will look for missing zlayers if any, graph traversal logic in zio will look for any missing
//    ConnectionPool.live(10)
//    ZLayer.Debug.tree  // will print out all layers with dependencies
//    ZLayer.Debug.mermaid // will print out a graph can only use either tree or mermaid
//  )
// ZIO will also tell you if you have multiple layers of the same type
//


  // magic v2
//  val userSubscriptionLayer_v2 = ZLayer.make[UserSubscription](
//    UserSubscription.live,
//    EmailService.live,
//    UserDatabase.live,
//    ConnectionPool.live(10)
//  )

  // passthrough, expose passed in dependency to the output of Zlayer
  val dbWithPoolLayer: ZLayer[ConnectionPool, Nothing, ConnectionPool with UserDatabase] = UserDatabase.live.passthrough
  // service = take a dep and expose it as a value to further layers
  val dbService = ZLayer.service[UserDatabase]
  // launch = creates a ZIO that uses the services and never finishes
  val subscriptionLaunch: ZIO[EmailService with UserDatabase, Nothing, Nothing] = UserSubscription.live.launch
  // memoization
  // will use the same needed object across application unless you append .fresh to it,

  /*

 Already provides services: Clock, Random, System, Console
   */

  val getTime = Clock.currentTime(TimeUnit.SECONDS)
  val randomValue = Random.nextInt
  val sysVariable = System.env("HADOOP_HOME")
  val printlnEffect = Console.printLine("This is ZIO")



  def run = runnableProgram

//    program_v2.provideLayer(
//    ZLayer.succeed(
//      UserSubscription.create(
//        EmailService.create(),
//        UserDatabase.create(
//          ConnectionPool.create(10)
//        )
//      )
//    )
//  )


}
