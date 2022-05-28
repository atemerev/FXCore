package ai.reactivity.fxcore.market

import ai.reactivity.fxcore.SafeDouble
import ai.reactivity.fxcore.instrument.{CurrencyPair, Instrument}

case class Quote(instrument: Instrument,
                 bid: Option[SafeDouble],
                 ask: Option[SafeDouble]) {

  val isFull: Boolean = bid.isDefined && ask.isDefined

  lazy val average: Option[SafeDouble] = if (isFull) Some((bid.get + ask.get) / 2) else None

  lazy val spread: Option[SafeDouble] = for (b <- bid; a <- ask) yield a - b

  def apply(side: QuoteSide.Value): Option[SafeDouble] = value(side)

  def value(side: QuoteSide.Value): Option[SafeDouble] = if (side == QuoteSide.Bid) bid else ask

  lazy val reverse: Quote = {
    val rBid: Option[SafeDouble] = for (a <- ask) yield 1 / a
    val rAsk: Option[SafeDouble] = for (b <- bid) yield 1 / b
    Quote(instrument.asInstanceOf[CurrencyPair].reverse, rBid, rAsk)
  }

  override def toString: String = {
    val bidS = (for (b <- bid) yield b.toString()).getOrElse("?")
    val askS = (for (a <- ask) yield a.toString()).getOrElse("?")
    "%s %s/%s".format(instrument, bidS, askS)
  }
}

object QuoteSide extends Enumeration {
  val Bid, Ask = Value

  def reverse(side: QuoteSide.Value): QuoteSide.Value = side match {
    case Bid => Ask
    case Ask => Bid
  }
}
