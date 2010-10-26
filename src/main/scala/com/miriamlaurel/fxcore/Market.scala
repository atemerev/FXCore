package com.miriamlaurel.fxcore

import java.util.Date
import scala.math.max
import com.miriamlaurel.fxcore.numbers.{Monetary, Zilch, Money, Decimal}
import java.io.Serializable

/**
 * @author Alexander Temerev
 */
class Market(lanes: Seq[Lane], val pivot: CurrencyAsset = CurrencyAsset("USD")) extends Serializable {

  val quotesMap = Map[Instrument, Lane](lanes.map(l => (l.instrument, l)): _*)

  def apply(instrument: Instrument): Lane = quotesMap(instrument)

  def lane(instrument: Instrument): Option[Lane] = quotesMap.get(instrument)

  def <<(lane: Lane): Market = {
    val newMap = quotesMap + (lane.instrument -> lane)
    new Market(newMap.values.toSeq)
  }

  def quote(instrument: Instrument, amount: Decimal): Option[Quote] = {
    if (instrument.primary == instrument.secondary)
      Some(new Quote(instrument, Some(1), Some(1), new Date)) else
    if (quotesMap.contains(instrument)) Some(apply(instrument).quote(amount)) else
    if (quotesMap.contains(instrument.reverse)) Some(apply(instrument.reverse).quote(amount).reverse) else
      for (a <- quoteToPivot(instrument.primary);
           b <- quoteToPivot(instrument.secondary);
           x <- Some(if (isReverse(a.instrument)) a.reverse else a);
           y <- Some(if (isStraight(b.instrument)) b.reverse else b);
           aBid <- x.bid;
           bBid <- y.bid;
           aAsk <- x.ask;
           bAsk <- y.ask;
           bid <- Some(aBid * bBid);
           ask <- Some(aAsk * bAsk)
      ) yield {
        new Quote(instrument, Some(bid), Some(ask), new Date(max(a.timestamp.getTime, b.timestamp.getTime)))
      }
  }

  def bestQuote(instrument: Instrument): Option[Quote] = quote(instrument, Decimal(0))

  def convert(from: Money,
              to: Asset,
              side: OfferSide.Value,
              amount: Decimal = 0): Option[Money] = from match {
      case Zilch => Some(Zilch)
      case m: Monetary => for (q <- quote(Instrument(m.asset, to), amount);
         p <- q.apply(side)) yield Money(p * m.amount, to)
  }

  private def quoteToPivot(asset: Asset): Option[Quote] = {
    val straight = Instrument(asset, pivot)
    val reverse = Instrument(pivot, asset)
    if (lane(straight).isDefined) Some(apply(straight).bestQuote) else
    if (lane(reverse).isDefined) Some(apply(reverse).bestQuote) else None
  }

  private def isStraight(instrument: Instrument) = pivot == instrument.secondary

  private def isReverse(instrument: Instrument) = pivot == instrument.primary
}

object Market {
  def apply(lanes: Lane*) = new Market(lanes)
}