package com.miriamlaurel.fxcore.market

import com.miriamlaurel.fxcore._
import com.miriamlaurel.fxcore.instrument.{CurrencyPair, Instrument}
import java.time.Instant

case class Quote(
        instrument: Instrument,
        bid: Option[BigDecimal],
        ask: Option[BigDecimal],
        override val timestamp: Instant) extends Timestamp {

  val isFull: Boolean = bid.isDefined && ask.isDefined

  lazy val average: Option[BigDecimal] = if (isFull) Some((bid.get + ask.get) / 2) else None

  lazy val spread: Option[BigDecimal] = for(b <- bid; a <- ask) yield a - b

  lazy val spreadPips: Option[BigDecimal] = instrument match {
    case cp: CurrencyPair ⇒ for (s <- spread) yield s / pipValue(cp)
    case _ ⇒ None
  }

  def apply(side: QuoteSide.Value): Option[BigDecimal] = value(side)

  def value(side: QuoteSide.Value): Option[BigDecimal] = if (side == QuoteSide.Bid) bid else ask

  lazy val reverse: Quote = {
    val rBid = for (a <- ask) yield BigDecimal(1) / a
    val rAsk = for (b <- bid) yield BigDecimal(1) / b
    Quote(instrument.asInstanceOf[CurrencyPair].reverse, rBid, rAsk, timestamp)
  }

  def normalize(fractionDigits: Int): Quote = {
    val nBid = for (b <- bid) yield rescale(b, pipScale(instrument.asInstanceOf[CurrencyPair]) + fractionDigits)
    val nAsk = for (a <- ask) yield rescale(a, pipScale(instrument.asInstanceOf[CurrencyPair]) + fractionDigits)
    Quote(instrument, nBid, nAsk, timestamp)
  }
}

object QuoteSide extends Enumeration {
  val Bid, Ask = Value

  def reverse(side: QuoteSide.Value): QuoteSide.Value = side match {
    case Bid ⇒ Ask
    case Ask ⇒ Bid
  }
}
