package catsByExamples.section2

import jdk.internal.vm.vector.VectorSupport.VectorPayload

object UsingMonads {
  import cats.Monad
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(45)
  val aChangedEither = loadingMonad.flatMap(anEither)(n =>
    if (n % 2 == 0) Right(n + 1) else Left("Loading meaning of Life ")
  )

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(
    OrderStatus(orderId, "Ready to ship")
  )

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000)
      Left("Not available yet, refreshing data...")
    else Right("Bangalore, NL")

  val orderId = 457L
  val orderLocation =
    loadingMonad.flatMap((getOrderStatus(orderId)))(orderStatus =>
      trackLocation(orderStatus)
    )

  // use extension method
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  val orderLocation_v2: LoadingOr[String] =
    getOrderStatus((orderId)).flatMap(trackLocation)
  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus((orderId))
    location <- trackLocation(orderStatus)
  } yield location

  // Exercise 1
  case class Connection(host: String, port: String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[F[_]] {
    def getConnection(cfg: Map[String, String]): F[Connection]
    def issueRequest(connection: Connection, payload: String): F[String]
  }
  // DO NOT CHANGE THE CODE
  /*
  Requirements:
  -if the host and port are found in configuration map, then we will return a F containing a connection with those
  values otherwise the method will fail, according to the logic of type M. (for Try it will return a Failure,
  for Option it will return None, for Future it will be a failed Future, for Either it will return a left
  - the issueRequest method returns a F containing the string: "request (payload) has ben accepted" , if the payload
  is less than 20 characters otherwise the method will fail, according to the logic of the type F

  TODO : provide real implementation of HttpService using Try, Option, Future, Either
   */
  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        h <- cfg.get("host")
        p <- cfg.get("port")
      } yield Connection(h, p)

    override def issueRequest(
        connection: Connection,
        payload: String
    ): Option[String] = if (payload.length >= 20) None
    else Some(s"Request $payload has been accepted")
  }
  // TODO implement another Http Service with LoadingOr or ErrorOr

  object ErrorOrHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      if (!cfg.contains("host") || !cfg.contains("port")) {
        Left(new RuntimeException("Invalid configuration"))
      } else Right(Connection(cfg.get("host").get, cfg.get("port").get))

    override def issueRequest(
        connection: Connection,
        payload: String
    ): ErrorOr[String] = if (payload.length >= 20)
      Left(new RuntimeException("over sized payload"))
    else Right(s"Request ($payload) has been accepted")
  }

  def getResponse[F[_]: Monad] (service: HttpService[F], payload: String): F[String] =
    for {
      connection <- service.getConnection(config)
      response <- service.issueRequest(connection, payload)
    } yield response
  def main(args: Array[String]): Unit = {
    println(anEither)
    println(aChangedEither)
    val responseOption = OptionHttpService.getConnection(config).flatMap {
      conn => OptionHttpService.issueRequest(conn, "Option HTTP service")
    }
    val responseEither = ErrorOrHttpService.getConnection(config).flatMap {
      conn => ErrorOrHttpService.issueRequest(conn, "Option HTTP service")
    }
    println(responseOption)
    println(responseEither)
    println(getResponse(ErrorOrHttpService, "Option HTTP service"))
    println(getResponse(OptionHttpService, "Option HTTP service"))
  }

}
