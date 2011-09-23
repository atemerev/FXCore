package com.miriamlaurel.fxcore

import java.util.Date
import com.miriamlaurel.fxcore.numbers.Decimal
import java.io.Serializable

/**
 * @author Alexander Temerev
 */
case class Lane(
  instrument: Instrument,
  allOffers: List[Offer],
  override val timestamp: Date) extends TimeEvent with Serializable {

  def this(instrument: Instrument, allOffers: List[Offer]) =
    this(instrument, allOffers, allOffers.map(_.timestamp).foldLeft(new Date)(Lane.min(_, _)))

  lazy val offers = allOffers.sortWith((a, b) => b.price > a.price)

  lazy val bids = offers.filter(_.side == OfferSide.Bid).reverse
  lazy val asks = offers.filter(_.side == OfferSide.Ask)

  private lazy val bestBid = if (bids.size > 0) Some(bids(0).price) else None
  private lazy val bestAsk = if (asks.size > 0) Some(asks(0).price) else None

  val bestQuote = new Quote(instrument, bestBid, bestAsk, timestamp)

  def selectSource(source: String) =
    new Lane(instrument, offers.filter(_.source == source))

  def slice(side: OfferSide.Value, amount: Decimal): List[Offer] = {
    var sum = Decimal(0)
    var enough = false
    val offers = if (side == OfferSide.Bid) bids else asks
    // I'd prefer inclusive takeWhile, but...
    offers.takeWhile({offer => enough = sum < amount; sum += offer.amount; enough})
  }

  def trim(amount: Decimal): Lane =
    new Lane(instrument, slice(OfferSide.Bid, amount) ::: slice(OfferSide.Ask, amount), timestamp)

  def trim(size: Int): Lane = new Lane(instrument, bids.take(size) ++ asks.take(size))

  def quote(amount: Decimal): Quote = {
    require(amount >= 0)
    if (amount == 0) bestQuote else {
      val sliceBid = slice(OfferSide.Bid, amount)
      val sliceAsk = slice(OfferSide.Ask, amount)
      val bid = if (sliceBid.size > 0) Some(weightedAvg(sliceBid)) else None
      val ask = if (sliceAsk.size > 0) Some(weightedAvg(sliceAsk)) else None
      new Quote(instrument, bid, ask, timestamp)
    }
  }

  def apply(id: String) = offers.find(_.id == id)

  def -(id: String) = new Lane(instrument, offers.filter(_.id != id), timestamp)

  def +(offer: Offer): Lane = new Lane(instrument, offers.filterNot(offers.filter(_.id == offer.id) contains), timestamp)

  def ++(lane: Lane): Lane = {
    val ms = Set[String]()
    new Lane(instrument, offers.foldLeft(List[Offer]())((a, b) => if (ms contains b.id) a else b :: a), timestamp)
  }

  override def toString = Lane.toCsv(this)

  private def weightedAvg(offers: List[Offer]): Decimal =
    offers.map(offer => offer.price * offer.amount).reduceLeft(_ + _) /
            offers.map(offer => offer.amount).reduceLeft(_ + _)

}

object Lane {

  def apply(s: String) = fromCsv(s)

  def fromCsv(csv: String): Lane = {
    def pair[A](l: List[A]): List[(A, A)] = l.grouped(2).collect {case List(a, b) => (a, b)}.toList
    val tokens = csv.split(",")
    val ts = new Date(tokens(0).toLong)
    val instrument = CurrencyPair(tokens(1))
    val asksIndex = tokens.indexOf("ASKS")
    val bidS: List[(String, String)] = pair(tokens.slice(3, asksIndex).toList)
    val askS: List[(String, String)] = pair(tokens.slice(asksIndex + 1, tokens.length).toList)
    val offers = bidS.map(n => new Offer("XX", instrument, OfferSide.Bid, Decimal(n._2), Decimal(n._1), ts)) ++
            askS.map(n => new Offer("XX", instrument, OfferSide.Ask, Decimal(n._2), Decimal(n._1), ts))
    Lane(instrument, offers, ts)
  }

  def toCsv(lane: Lane) = {
    lane.timestamp.getTime.toString + "," + lane.instrument.toString + ",BIDS," +
            lane.bids.reverse.map(o => o.price.toString + "," + o.amount.toString).mkString(",") +
            ",ASKS," +
            lane.asks.map(o => o.price.toString + "," + o.amount.toString).mkString(",")
  }

  private def min(d1: Date, d2: Date) = if (d2.compareTo(d1) >= 0) d1 else d2

  private def max(d1: Date, d2: Date) = if (d2.compareTo(d1) < 0) d2 else d1
}
