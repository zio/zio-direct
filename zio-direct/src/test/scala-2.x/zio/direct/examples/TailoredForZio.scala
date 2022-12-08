package zio.direct.examples

import java.io.BufferedReader
import java.io.FileReader
import zio.ZIO
import zio.direct._
import zio.direct.core.metaprog.Verify
import java.io.IOException
import zio.Ref
import zio.Chunk

object TailoredForZio {

  case class CustomerConfig(url: String)
  case class DistributorConfig(url: String)

  case class Customer(firstName: String, last: String)
  case class Distributor(id: Int, street: String)

  def httpGet(url: String): ZIO[Any, IOException, String] = ???
  def parseCustomer(customer: String): Customer = ???
  def parseDistrubutor(customer: String): Distributor = ???

  class CustomerGetException extends Exception("customer GET wrong")
  class DistrubutorGetException extends Exception("distributor GET wrong")

  def httpGetCustomer(url: String): ZIO[Any, CustomerGetException, String] = ???
  def httpGetDistributor(url: String): ZIO[Any, DistrubutorGetException, String] = ???

  // Things to talk about
  // throws becomes ZIO.fail
  // mutable refs (include the sorting example)
  // lenient mode is still correct
  // low commitment API

  {
    val out =
      defer {
        val custUrl: String = ZIO.service[CustomerConfig].run.url
        val distUrl: String = ZIO.service[DistributorConfig].run.url
        (
          parseCustomer(httpGetCustomer(custUrl).run),
          parseDistrubutor(httpGetDistributor(distUrl).run)
        )
      }
  }
  {
    ZIO.service[CustomerConfig].flatMap { cc =>
      val custUrl: String = cc.url
      ZIO.service[DistributorConfig].flatMap { dc =>
        val distUrl: String = dc.url
        ZIO.collectAll(Chunk(httpGet(custUrl), (httpGet(distUrl)))).map { col =>
          val iter = col.iterator
          (parseCustomer(iter.next()), parseDistrubutor(iter.next()))
        }
      }
    }
  }

}
