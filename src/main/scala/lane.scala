package com.miriamlaurel.fxcore

import com.miriamlaurel.fxcore.numbers.Decimal
import java.io.Serializable
import java.util.UUID

/**
 * @author Alexander Temerev
 */
case class Lane(
  instrument: Instrument,
  allOffers: List[Offer],
  override val timestamp: Long) extends TimeEvent with Serializable {

  def this(instrument: Instrument, allOffers: List[Offer]) =
    this(instrument, allOffers, allOffers.map(_.timestamp).reduceLeft((ts1, ts2) => ts1 min ts2))

  lazy val offers = allOffers.sortWith((a, b) => b.price > a.price)

  lazy val bids = offers.filter(_.side == OfferSide.Bid).reverse
  lazy val asks = offers.filter(_.side == OfferSide.Ask)

  lazy val bestBid: Option[Decimal] = if (bids.size > 0) Some(bids(0).price) else None
  lazy val bestAsk: Option[Decimal] = if (asks.size > 0) Some(asks(0).price) else None

  lazy val bestQuote = Quote(instrument, bestBid, bestAsk, timestamp)

  def selectSource(source: String): Lane = Lane(instrument, offers.filter(_.source == source), timestamp)

  def isFull: Boolean = bids.size > 0 && asks.size > 0

  def slice(side: OfferSide.Value, amount: Decimal): List[Offer] = {
    var sum = Decimal(0)
    var enough = false
    val offers = if (side == OfferSide.Bid) bids else asks
    // I'd prefer inclusive takeWhile, but...
    offers.takeWhile({offer => enough = sum < amount; sum += offer.amount; enough})
  }

  def trim(amount: Decimal): Lane =
    Lane(instrument, slice(OfferSide.Bid, amount) ::: slice(OfferSide.Ask, amount), timestamp)

  def trim(size: Int): Lane = new Lane(instrument, bids.take(size) ++ asks.take(size))

  def quote(amount: Decimal): Quote = {
    require(amount >= 0)
    if (amount == 0) bestQuote else {
      val sliceBid = slice(OfferSide.Bid, amount)
      val sliceAsk = slice(OfferSide.Ask, amount)
      val bid = if (sliceBid.size > 0) Some(weightedAvg(sliceBid)) else None
      val ask = if (sliceAsk.size > 0) Some(weightedAvg(sliceAsk)) else None
      Quote(instrument, bid, ask, timestamp)
    }
  }

  def apply(uuid: UUID) = offers.find(_.uuid == uuid)

  def -(uuid: UUID) = Lane(instrument, offers.filter(_.uuid != uuid), timestamp)

  def +(offer: Offer): Lane = Lane(instrument, offers.filterNot(offers.filter(_.uuid == offer.uuid) contains), timestamp)

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
    val ts = tokens(0).toLong
    val instrument = CurrencyPair(tokens(1))
    val asksIndex = tokens.indexOf("ASKS")
    val bidS: List[(String, String)] = pair(tokens.slice(3, asksIndex).toList)
    val askS: List[(String, String)] = pair(tokens.slice(asksIndex + 1, tokens.length).toList)
    val offers = bidS.map(n => new Offer("XX", instrument, OfferSide.Bid, Decimal(n._2), Decimal(n._1), UUID.randomUUID(), ts)) ++
            askS.map(n => new Offer("XX", instrument, OfferSide.Ask, Decimal(n._2), Decimal(n._1), UUID.randomUUID(), ts))
    Lane(instrument, offers, ts)
  }

  def toCsv(lane: Lane) = {
    lane.timestamp.toString + "," + lane.instrument.toString + ",BIDS," +
            lane.bids.reverse.map(o => o.price.toString + "," + o.amount.toString).mkString(",") +
            ",ASKS," +
            lane.asks.map(o => o.price.toString + "," + o.amount.toString).mkString(",")
  }
}
