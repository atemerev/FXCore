package com.miriamlaurel.fxcore

import java.util.UUID

case class MarketEntry(instrument: Instrument,
                       side: QuoteSide.Value,
                       amount: BigDecimal,
                       price: BigDecimal,
                       source: Party = Me,
                       sourceId: Option[String] = None,
                       override val timestamp: Long = System.currentTimeMillis(),
                       override val uuid: UUID = UUID.randomUUID()) extends Ordered[MarketEntry] with Entity with TimeEvent {

  require(amount > 0)
  require(price > 0)

  override def compare(that: MarketEntry) = price compare that.price
  override def toString = "%s %f %s @%f".format(side.toString, amount.bigDecimal, instrument.toString, price.bigDecimal)
}

case class Snapshot(
  instrument: Instrument,
  allEntries: List[MarketEntry],
  override val timestamp: Long) extends TimeEvent {

  def this(instrument: Instrument, allOrders: List[MarketEntry]) =
    this(instrument, allOrders, allOrders.map(_.timestamp).reduceLeft((ts1, ts2) => ts1 min ts2))

  lazy val entries = allEntries.sorted

  lazy val bids = entries.filter(_.side == QuoteSide.Bid).reverse
  lazy val asks = entries.filter(_.side == QuoteSide.Ask)

  lazy val bestBid: Option[BigDecimal] = if (bids.size > 0) Some(bids(0).price) else None
  lazy val bestAsk: Option[BigDecimal] = if (asks.size > 0) Some(asks(0).price) else None

  lazy val best = Quote(instrument, bestBid, bestAsk, timestamp)

  def selectSource(source: Party): Snapshot = Snapshot(instrument, entries.filter(_.source == source), timestamp)

  def isFull: Boolean = bids.size > 0 && asks.size > 0

  def slice(side: QuoteSide.Value, amount: BigDecimal): List[MarketEntry] = {
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
      val bid = if (sliceBid.size > 0) Some(weightedAvg(sliceBid)) else None
      val ask = if (sliceAsk.size > 0) Some(weightedAvg(sliceAsk)) else None
      Quote(instrument, bid, ask, timestamp)
    }
  }

  def +(order: MarketEntry): Snapshot = copy(allEntries = this.allEntries.::(order))

  override def toString = Snapshot.toCsv(this)

  private def weightedAvg(orders: List[MarketEntry]): BigDecimal =
    orders.map(order => order.price * order.amount).reduceLeft(_ + _) /
            orders.map(_.amount).reduceLeft(_ + _)

}

object Snapshot {

  def apply(s: String) = fromCsv(s)

  def fromCsv(csv: String): Snapshot = {
    def pair[A](l: List[A]): List[(A, A)] = l.grouped(2).collect {case List(a, b) => (a, b)}.toList
    val tokens = csv.split(",")
    val ts = tokens(0).toLong
    val instrument = CurrencyPair(tokens(1))
    val asksIndex = tokens.indexOf("ASKS")
    val bidS: List[(String, String)] = pair(tokens.slice(3, asksIndex).toList)
    val askS: List[(String, String)] = pair(tokens.slice(asksIndex + 1, tokens.length).toList)
    val orders = bidS.map(
      n => new MarketEntry(instrument, QuoteSide.Bid, BigDecimal(n._2), BigDecimal(n._1), Me, None, ts)) ++
      askS.map(n => new MarketEntry(instrument, QuoteSide.Ask, BigDecimal(n._2), BigDecimal(n._1), Me, None, ts))
    Snapshot(instrument, orders, ts)
  }

  def toCsv(snapshot: Snapshot) = {
    snapshot.timestamp.toString + "," + snapshot.instrument.toString + ",BIDS," +
            snapshot.bids.reverse.map(o => o.price.toString + "," + o.amount.toString).mkString(",") +
            ",ASKS," +
            snapshot.asks.map(o => o.price.toString + "," + o.amount.toString).mkString(",")
  }
}
