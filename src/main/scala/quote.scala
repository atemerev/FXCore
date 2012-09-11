package com.miriamlaurel.fxcore

import com.miriamlaurel.fxcore.numbers.Decimal
import com.miriamlaurel.fxcore.pipscaler._

/**
 * @author Alexander Temerev
 */
case class Quote(
        instrument: Instrument,
        bid: Option[Decimal],
        ask: Option[Decimal],
        override val timestamp: Long) extends TimeEvent {

  val isFull: Boolean = bid.isDefined && ask.isDefined

  lazy val average: Option[Decimal] = if (isFull) (Some((bid.get + ask.get) / 2)) else None

  lazy val spread: Option[Decimal] = for(b <- bid; a <- ask) yield a - b

  lazy val spreadPips: Option[Decimal] = instrument match {
    case cp: CurrencyPair => for (s <- spread) yield s / pipValue(cp)
    case _ => None
  }

  def apply(side: OfferSide.Value): Option[Decimal] = value(side)

  def value(side: OfferSide.Value): Option[Decimal] = if (side == OfferSide.Bid) bid else ask

  lazy val reverse: Quote = {
    val rBid = for (a <- ask) yield a.reciprocal
    val rAsk = for (b <- bid) yield b.reciprocal
    Quote(instrument.asInstanceOf[CurrencyPair].reverse, rBid, rAsk, timestamp)
  }

  def normalize(fractionDigits: Int): Quote = {
    val nBid = for (b <- bid) yield rescale(b, pipScale(instrument.asInstanceOf[CurrencyPair]) + fractionDigits)
    val nAsk = for (a <- ask) yield rescale(a, pipScale(instrument.asInstanceOf[CurrencyPair]) + fractionDigits)
    Quote(instrument, nBid, nAsk, timestamp)
  }
}