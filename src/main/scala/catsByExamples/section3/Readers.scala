package catsByExamples.section3

object Readers {
  /*
  - configuration file => initial data structure
  - a DB layer
   - an HTTP layer
   - a business logic layer
   */

  case class Configuration(
      dbUsername: String,
      dbPassword: String,
      host: String,
      port: Int,
      nThreads: Int,
      emailReplyTo: String
  )
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched"
    def getLastOrderId(username: String): Long = 5432
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started")
  }

  // bootstrap
  val config =
    Configuration("sabuj", "mypasswd", "localhost", 123, 8, "sabuj@xyz.com")

  // cats Reader

  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] =
    Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))

  val dbConn = dbReader.run(config)

  // Reader[I,O]  map: O => O'
  val sabujOrderStatusReader: Reader[Configuration, String] =
    dbReader.map(dbcon => dbcon.getOrderStatus((55)))
  val sabujOrderStatus: String = sabujOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val userLastOrderIdReader: Reader[Configuration, String] = dbReader
      .map(dbCon => dbCon.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))
    val userLastOrderIdReader_v2: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus
    userLastOrderIdReader.run(config)
    userLastOrderIdReader_v2.run(config)
  }

  /*
  Pattern
  1.  You create the initial data structure
  2. You create a reader which specifies how that data structure will be manupulated later
  3. You can then map & flatMap the reader to produce derived information
  4. When you need the final piece of information, you call run on the reader with the initial data structure
   */

  // TODO
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) =
      s"From: $emailReplyTo; to: $address >>> $contents"
  }

  def emailUser(username: String, userEmail: String) = {
    // fetch the status of their last order
    // email them with the Email service: " Your last order has the status: (status)"

    val emailServiceReader: Reader[Configuration, EmailService] =
      Reader(conf => EmailService(conf.emailReplyTo))
    val emailReader: Reader[Configuration, String] =
      for {
        lastOrderId <- dbReader.map(_.getLastOrderId(username))
        orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
        emailService <- emailServiceReader
      } yield emailService.sendEmail(
        userEmail,
        s"Your last order has status: $orderStatus"
      )
    emailReader.run(config)
  }

  // TODO 2: what programming patter do Readers remind you off?
  // Dependency Injection
  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("sabuj"))
    println(emailUser("sabuj", "sabuj@abc.com"))
  }

}
