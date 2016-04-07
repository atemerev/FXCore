package com.miriamlaurel.fxcore.market

import com.miriamlaurel.fxcore.instrument.{CurrencyPair, Instrument}
import java.time.Instant
import com.miriamlaurel.fxcore.{Me, Timestamp}
import com.miriamlaurel.fxcore.party.Party

case class Snapshot(
  instrument: Instrument,
  allEntries: List[Order],
  override val timestamp: Instant) extends Timestamp {

  def this(instrument: Instrument, allOrders: List[Order]) =
    this(instrument, allOrders, Instant.now())

  lazy val entries = allEntries.sorted

  lazy val bids = entries.filter(_.key.side == QuoteSide.Bid)
  lazy val asks = entries.filter(_.key.side == QuoteSide.Ask)

  lazy val bestBid: Option[BigDecimal] = if (bids.nonEmpty) Some(bids.head.price) else None
  lazy val bestAsk: Option[BigDecimal] = if (asks.nonEmpty) Some(asks.head.price) else None

  lazy val best = Quote(instrument, bestBid, bestAsk, timestamp)

  def selectSource(source: Party): Snapshot = Snapshot(instrument, entries.filter(_.key.party == source), timestamp)

  def isFull: Boolean = bids.nonEmpty && asks.nonEmpty

  def slice(side: QuoteSide.Value, amount: BigDecimal): List[Order] = {
    var sum = BigDecimal(0)
    var enough = false
    val orders = if (side == QuoteSide.Bid) bids else asks
    // I'd prefer inclusive takeWhile, but...
    orders.takeWhile({order => enough = sum < amount; sum += order.amount; enough})
  }

  def trim(amount: BigDecimal): Snapshot =
    Snapshot(instrument, slice(QuoteSide.Bid, amount) ::: slice(QuoteSide.Ask, amount), timestamp)

  def trim(size: Int): Snapshot = new Snapshot(instrument, bids.take(size) ++ asks.take(size))

  def quote(amount: BigDecimal): Quote = {
    require(amount >= 0)
    if (amount == BigDecimal(0)) best else {
      val sliceBid = slice(QuoteSide.Bid, amount)
      val sliceAsk = slice(QuoteSide.Ask, amount)
      val bid = if (sliceBid.nonEmpty) Some(weightedAvg(sliceBid)) else None
      val ask = if (sliceAsk.nonEmpty) Some(weightedAvg(sliceAsk)) else None
      Quote(instrument, bid, ask, timestamp)
    }
  }

  def +(order: Order): Snapshot = copy(allEntries = this.allEntries.::(order))

  override def toString = Snapshot.toCsv(this)

  private def weightedAvg(orders: List[Order]): BigDecimal =
    orders.map(order => order.price * order.amount).sum / orders.map(_.amount).sum
}

object Snapshot {

  def apply(s: String) = fromCsv(s)

  def fromCsv(csv: String): Snapshot = {
    def pair[A](l: List[A]): List[(A, A)] = l.grouped(2).collect {case List(a, b) => (a, b)}.toList
    val tokens = csv.split(",")
    val ts = Instant.ofEpochMilli(tokens(0).toLong)
    val instrument = CurrencyPair(tokens(1))
    val asksIndex = tokens.indexOf("ASKS")
    val bidS: List[(String, String)] = pair(tokens.slice(3, asksIndex).toList)
    val askS: List[(String, String)] = pair(tokens.slice(asksIndex + 1, tokens.length).toList)
    val orders = bidS.map(
      n => Order(Me, instrument, QuoteSide.Bid, "*", BigDecimal(n._2), BigDecimal(n._1))) ++
      askS.map(n => Order(Me, instrument, QuoteSide.Ask, "*", BigDecimal(n._2), BigDecimal(n._1)))
    Snapshot(instrument, orders, ts)
  }

  def toCsv(snapshot: Snapshot) = {
    snapshot.timestamp.toEpochMilli + "," + snapshot.instrument.toString + ",BIDS," +
            snapshot.bids.reverse.map(o => o.price.toString + "," + o.amount.toString).mkString(",") +
            ",ASKS," +
            snapshot.asks.map(o => o.price.toString + "," + o.amount.toString).mkString(",")
  }
}
