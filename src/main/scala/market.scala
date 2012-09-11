package com.miriamlaurel.fxcore

import scala.math.max
import com.miriamlaurel.fxcore.numbers.{Monetary, Zilch, Money, Decimal}

/**
 * @author Alexander Temerev
 */
class Market(lanes: Seq[Lane], val pivot: Currency = Currency("USD")) {

  private val booksMap = Map[Instrument, Lane](lanes.map(l => (l.instrument, l)): _*)

  lazy val timestamp = lanes.map(_.timestamp).max

  def apply(instrument: Instrument): Lane = booksMap(instrument)

  def lane(instrument: Instrument): Option[Lane] = booksMap.get(instrument)

  def <<(lane: Lane): Market = {
    val newMap = booksMap + (lane.instrument -> lane)
    new Market(newMap.values.toSeq)
  }

  def quote(instrument: Instrument, amount: Decimal): Option[Quote] = {
    if (instrument.base == instrument.counter)
      Some(Quote(instrument, Some(1), Some(1), timestamp)) else
    if (booksMap.contains(instrument)) Some(apply(instrument).quote(amount)) else
    if (booksMap.contains(instrument.reverse)) Some(apply(instrument.reverse).quote(amount).reverse) else {
      for (a <- quoteToPivot(instrument.base);
           b <- quoteToPivot(instrument.counter);
           x <- Some(if (isReverse(a.instrument)) a.reverse else a);
           y <- Some(if (isStraight(b.instrument)) b.reverse else b);
           aBid <- x.bid;
           bBid <- y.bid;
           aAsk <- x.ask;
           bAsk <- y.ask;
           bid <- Some(aBid * bBid);
           ask <- Some(aAsk * bAsk)
      ) yield Quote(instrument, Some(bid), Some(ask), max(a.timestamp, b.timestamp))
    }
  }

  def bestQuote(instrument: Instrument): Option[Quote] = quote(instrument, Decimal(0))

  def convert(from: Money,
              to: AssetClass,
              side: OfferSide.Value,
              amount: Decimal = 0): Option[Money] = from match {
      case Zilch => Some(Zilch)
      case m: Monetary => for (q <- quote(Instrument(m.asset, to), amount);
         p <- q.apply(side)) yield Money(p * m.amount, to)
  }

  private def quoteToPivot(asset: AssetClass): Option[Quote] = {
    val straight = Instrument(asset, pivot)
    val reverse = Instrument(pivot, asset)
    if (lane(straight).isDefined) Some(apply(straight).bestQuote) else
    if (lane(reverse).isDefined) Some(apply(reverse).bestQuote) else None
  }

  private def isStraight(instrument: Instrument) = pivot == instrument.counter

  private def isReverse(instrument: Instrument) = pivot == instrument.base
}

object Market {
  def apply(lanes: Lane*) = new Market(lanes)
}