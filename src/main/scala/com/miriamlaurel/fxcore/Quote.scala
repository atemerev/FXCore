package com.miriamlaurel.fxcore

import java.util.Date
import com.miriamlaurel.fxcore.numbers.Decimal
import com.miriamlaurel.fxcore.pipscaler._

/**
 * @author Alexander Temerev
 */
class Quote(
        val instrument: Instrument,
        val bid: Option[Decimal],
        val ask: Option[Decimal],
        override val timestamp: Date) extends TimeEvent {
  
  def average = if (isFull) (Some((bid.get + ask.get) / 2)) else None

  def isFull = bid.isDefined && ask.isDefined

  def value(side: OfferSide.Value) = if (side == OfferSide.Bid) bid else ask

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

  override def toString = instrument + " " + bid.getOrElse("N/A") + " / " + ask.getOrElse("N/A")

  override def hashCode: Int = {
    val bidHash = if (bid.isDefined) bid.hashCode else 0;
    val askHash = if (ask.isDefined) ask.hashCode else 0;
    return 29 * (instrument.hashCode + 29 * (bidHash + 29 * askHash))
  }

  override def equals(obj: Any) = {
    obj.isInstanceOf[Quote] &&
            instrument == obj.asInstanceOf[Quote].instrument &&
            bid == obj.asInstanceOf[Quote].bid &&
            ask == obj.asInstanceOf[Quote].ask
  }
}
