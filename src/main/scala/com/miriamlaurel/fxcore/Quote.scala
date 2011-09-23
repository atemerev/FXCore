package com.miriamlaurel.fxcore

import java.util.Date
import com.miriamlaurel.fxcore.numbers.Decimal
import com.miriamlaurel.fxcore.pipscaler._

/**
 * @author Alexander Temerev
 */
case class Quote(
        instrument: Instrument,
        bid: Option[Decimal],
        ask: Option[Decimal],
        override val timestamp: Date) extends TimeEvent {
  
  def average = if (isFull) (Some((bid.get + ask.get) / 2)) else None

  def isFull = bid.isDefined && ask.isDefined

  def value(side: OfferSide.Value) = if (side == OfferSide.Bid) bid else ask

  def spread: Option[Decimal] = for(b <- bid; a <- ask) yield a - b

  def spreadPips: Option[Decimal] = instrument match {
    case cp: CurrencyPair => for (s <- spread) yield s / pipValue(cp)
    case _ => None
  }

  def apply(side: OfferSide.Value): Option[Decimal] = value(side)

  def reverse: Quote = {
    val rBid = for (a <- ask) yield a.reciprocal
    val rAsk = for (b <- bid) yield b.reciprocal
    new Quote(instrument.reverse, rBid, rAsk, timestamp)
  }

  def normalize(fractionDigits: Int) = {
    val nBid = for (b <- bid) yield rescale(b, pipScale(instrument.asInstanceOf[CurrencyPair]) + fractionDigits)
    val nAsk = for (a <- ask) yield rescale(a, pipScale(instrument.asInstanceOf[CurrencyPair]) + fractionDigits)
    new Quote(instrument, nBid, nAsk, timestamp)
  }
}
