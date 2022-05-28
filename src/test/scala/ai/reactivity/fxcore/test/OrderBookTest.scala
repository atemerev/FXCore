package ai.reactivity.fxcore.test

import ai.reactivity.fxcore._
import ai.reactivity.fxcore.instrument.CurrencyPair
import ai.reactivity.fxcore.market._
import ai.reactivity.fxcore.party.Party
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class OrderBookTest extends AnyFunSuite with Matchers {

  import OrderBookTest._

  val ts = 1273787999996L
  private val orderBook = fromCsv("1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.2522,1000000,1.2523,1000000,ASKS,1.2524,1000000,1.25246,1000000")

  test("trim should leave best orders totalling exactly the trim amount") {
    "1273787999996,EUR/USD,BIDS,1.2523,500000,ASKS,1.2524,500000" should equal(toCsv(orderBook.trim(SafeDouble(500000)), ts))
  }

  test("quote to a limit should respect trimming rules") {
    orderBook.quote(1) should equal(orderBook.best)
    orderBook.quote(2000000) should equal(Quote(orderBook.instrument, Some(SafeDouble(1.25225)), Some(SafeDouble(1.25243))))
  }

  test("remove best bid from order book") {
    val newBook = orderBook removeOrder OrderKey(Me, EURUSD, QuoteSide.Bid, "0")
    "1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.2522,1000000,ASKS,1.2524,1000000,1.25246,1000000" should equal(toCsv(newBook, ts))
  }

  test("remove mid bid from order book") {
    val newBook = orderBook removeOrder OrderKey(Me, EURUSD, QuoteSide.Bid, "2")
    "1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.2522,1000000,1.2523,1000000,ASKS,1.2524,1000000,1.25246,1000000" should equal(toCsv(newBook, ts))
  }

  test("remove best ask from order book") {
    val newBook = orderBook removeOrder OrderKey(Me, EURUSD, QuoteSide.Ask, "0")
    "1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.2522,1000000,1.2523,1000000,ASKS,1.25246,1000000" should equal(toCsv(newBook, ts))
  }

  test("remove far ask from order book") {
    val newBook = orderBook removeOrder OrderKey(Me, EURUSD, QuoteSide.Ask, "1")
    "1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.2522,1000000,1.2523,1000000,ASKS,1.2524,1000000" should equal(toCsv(newBook, ts))
  }

  test("add mid bid to order book") {
    val newBook = orderBook addOrder Order(OrderKey(Me, EURUSD, QuoteSide.Bid, "*"), SafeDouble(500000), SafeDouble(1.25214))
    "1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.25214,500000,1.2522,1000000,1.2523,1000000,ASKS,1.2524,1000000,1.25246,1000000" should equal(toCsv(newBook, ts))
    val newBook2 = newBook addOrder Order(OrderKey(Me, EURUSD, QuoteSide.Bid, "*"), SafeDouble(1000000), SafeDouble(1.25214))
    "1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.25214,1000000,1.2522,1000000,1.2523,1000000,ASKS,1.2524,1000000,1.25246,1000000" should equal(toCsv(newBook2, ts))
    val newBook3 = newBook2 addOrder Order(OrderKey(Me, EURUSD, QuoteSide.Bid, "*2"), SafeDouble(1000), SafeDouble(1.25214))
    "1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.25214,1000,1.25214,1000000,1.2522,1000000,1.2523,1000000,ASKS,1.2524,1000000,1.25246,1000000" should equal(toCsv(newBook3, ts))
  }

  test("matcher smoke test") {
    val aggressiveOrder = Order(Me, BTC/USD, QuoteSide.Bid, "agg1", 1500000, 1.2525)
    val (book, matches: Seq[Match]) = orderBook.matchWith(aggressiveOrder, Seq.empty)
    matches.map(_.quantity).sum === aggressiveOrder.amount
    matches.size === 2
  }

}

object OrderBookTest {
  def fromCsv(csv: String): OrderBook = {
    def pair[A](l: List[A]): List[(A, A)] = l.grouped(2).collect { case List(a, b) => (a, b) }.toList
    val tokens = csv.split(",")
    val instrument = CurrencyPair(tokens(1))
    val asksIndex = tokens.indexOf("ASKS")
    val bidS: List[(String, String)] = pair(tokens.slice(3, asksIndex).toList)
    val askS: List[(String, String)] = pair(tokens.slice(asksIndex + 1, tokens.length).toList)
    val bidSize = bidS.size
    val orders = bidS.zipWithIndex.map(
      n => Order(Me, instrument, QuoteSide.Bid, (bidSize - n._2 - 1).toString, n._1._2.toDouble, n._1._1.toDouble)) ++
      askS.zipWithIndex.map(n => Order(Me, instrument, QuoteSide.Ask, n._2.toString, n._1._2.toDouble, n._1._1.toDouble))
    OrderBook(orders)
  }

  def toCsv(orderBook: OrderBook, timestamp: Long): String = {
    "" + timestamp + "," + orderBook.instrument.toString + ",BIDS," +
      orderBook.bids.values.flatMap(_.orders).toSeq.reverse.map(o => o.price.toString + "," + o.amount.toString).mkString(",") +
      ",ASKS," +
      orderBook.asks.values.flatMap(_.orders).map(o => o.price.toString + "," + o.amount.toString).mkString(",")
  }
}