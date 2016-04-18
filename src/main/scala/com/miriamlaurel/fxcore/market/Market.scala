package com.miriamlaurel.fxcore.market

import com.miriamlaurel.fxcore._
import com.miriamlaurel.fxcore.asset.{AssetClass, Currency}
import com.miriamlaurel.fxcore.instrument.Instrument
import java.time.Instant

case class Market(snapshots: Seq[OrderBook], pivot: Currency = USD) {

  private val content = Map[Instrument, OrderBook](snapshots.map(l => (l.instrument, l)): _*)

  lazy val timestamp = Instant.ofEpochMilli(snapshots.map(_.timestamp.toEpochMilli).max)

  def apply(instrument: Instrument): OrderBook = content(instrument)

  def snapshot(instrument: Instrument): Option[OrderBook] = content.get(instrument)

  def <<(snapshot: OrderBook): Market = {
    val newContent = content + (snapshot.instrument -> snapshot)
    Market(newContent.values.toSeq)
  }

  def quote(instrument: Instrument, amount: BigDecimal = 0): Option[Quote] = {
    if (instrument.base == instrument.counter)
      Some(Quote(instrument, Some(1), Some(1), timestamp)) else
    if (content.contains(instrument)) Some(apply(instrument).quote(amount)) else
    if (content.contains(instrument.reverse)) Some(apply(instrument.reverse).quote(amount).reverse) else {
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
      ) yield Quote(instrument, Some(bid), Some(ask), Instant.ofEpochMilli(a.timestamp.toEpochMilli max b.timestamp.toEpochMilli))
    }
  }

  def convert(from: Money,
              to: AssetClass,
              side: QuoteSide.Value,
              amount: BigDecimal = 0): Option[Money] = from match {
      case Zilch => Some(Zilch)
      case m: Monetary => for (q <- quote(Instrument(m.asset, to), amount);
         p <- q.apply(side)) yield Money(p * m.amount, to)
  }

  private def quoteToPivot(asset: AssetClass): Option[Quote] = {
    val straight = Instrument(asset, pivot)
    val reverse = Instrument(pivot, asset)
    if (snapshot(straight).isDefined) Some(apply(straight).best) else
    if (snapshot(reverse).isDefined) Some(apply(reverse).best) else None
  }

  private def isStraight(instrument: Instrument) = pivot == instrument.counter

  private def isReverse(instrument: Instrument) = pivot == instrument.base
}

object Market {
  def apply(snapshots: OrderBook*): Market = Market(snapshots.toSeq)
}
