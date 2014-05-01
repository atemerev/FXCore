package com.miriamlaurel.fxcore.portfolio

import java.util.UUID
import org.joda.time.DateTime
import com.miriamlaurel.fxcore._
import com.miriamlaurel.fxcore.instrument.{CurrencyPair, Instrument}
import com.miriamlaurel.fxcore.asset.AssetClass
import com.miriamlaurel.fxcore.market.{QuoteSide, Quote, Market}
import com.miriamlaurel.fxcore.accounting.Deal

/** Position
This is the implementation of a position taken in some asset against the other. For example, it can be
"-100 EUR / 137 USD", which probably means (dependent on context) that 100 euro had been short-sold for 137 US dollars.
Thus, a position has following fields:

  * primary: amount in primary asset, like "-100 EUR" in the example above;
  * secondary: amount in secondary asset, like "137 USD" in the example above;
  * matching: an UUID of a matching position in some portfolio. In some contexts, if a position is applied to portfolio
  with matching position, these positions will be merged;
  * timestamp: position's creation time;
  * uuid: this position's UUID.
  */
case class Position(primary: Monetary,
                    secondary: Monetary,
                    matching: Option[UUID],
                    override val timestamp: DateTime,
                    override val id: UUID)
  extends Identity with Timestamp {

  /*
  def this(instrument: Instrument, price: BigDecimal, amount: BigDecimal) =
    this(Monetary(amount, instrument.base), Monetary(-amount * price, instrument.counter))

  def this(instrument: Instrument, price: BigDecimal, amount: BigDecimal, matching: Option[UUID]) =
    this(Monetary(amount, instrument.base), Monetary(-amount * price, instrument.counter), matching)

  def this(instrument: Instrument, price: BigDecimal, amount: BigDecimal, matching: Option[UUID], timestamp: Long) =
    this(Monetary(amount, instrument.base), Monetary(-amount * price, instrument.counter), matching, timestamp)

  def this(instrument: Instrument, price: BigDecimal, amount: BigDecimal, matching: Option[UUID], timestamp: Long, id: UUID) =
    this(Monetary(amount, instrument.base), Monetary(-amount * price, instrument.counter),
      matching, timestamp, id)
  */

  /**
  If position's primary amount is positive (a long position), it's secondary amount should be negative,
  and vice versa.
    */
  require(primary.amount.signum != secondary.amount.signum)

  /*!
  An instrument can be constructed from position's primary and secondary asset, like "EUR/USD" (a currency pair).
   */
  lazy val instrument = Instrument(primary.asset, secondary.asset)

  /*!
  We can easily calculate a price for 1 unit of primary asset -- it is usually referred as position's price.
   */
  lazy val price: BigDecimal = (secondary.amount / primary.amount).abs

  /*!
  Position can be either long (primary amount is positive) or short (negative). Short positions may be handled
  differently dependent on asset class; in forex, long and short positions are usually quite symmetrical.
   */
  lazy val side = if (primary.amount > 0) PositionSide.Long else PositionSide.Short

  /*!
  Position amount is the absolute value of primary asset amount, i.e. it is always positive. If amount sign
  matters (it usually does), one should use .primary.amount
   */
  lazy val absoluteAmount: BigDecimal = primary.amount.abs

  /*!
  Create a reversed position with new price and matching UUID. This can be used efficiently to close a position.
   */
  def close(newPrice: BigDecimal): Position = Position(instrument, newPrice, -primary.amount, Some(id))

  /*!
  Position's profit or loss for any price level can be easily calculated, provided it's expressed in position's
  secondary asset units.
   */
  def profitLoss(newPrice: BigDecimal): Money = {
    Money((newPrice - price) * primary.amount, secondary.asset)
  }

  /*!
  For convenience, profit/loss in secondary asset units can also be calculated from a quote for position's
  instrument. If quote doesn't contain bid or ask price required for this position, None is returned.
   */
  def profitLoss(q: Quote): Option[Money] = {
    require(q.instrument == this.instrument, "Quote and position instrument should match")
    for (price <- if (side == PositionSide.Long) q.bid else q.ask) yield profitLoss(price)
  }

  /*!
  Profit/loss can be expressed in any asset, provided we have all quotes necessary for conversion from
  position's secondary asset to desired asset. A Market instance is required for that; if conversion can't
  be done for whatever reason, None is returned.
   */
  def profitLossIn(asset: AssetClass, market: Market): Option[Money] = {
    for (q <- market.quote(instrument, absoluteAmount);
         raw <- profitLoss(q);
         side = if (raw.amount >= 0) QuoteSide.Bid else QuoteSide.Ask;
         m <- market.convert(raw, asset, side, absoluteAmount)) yield m
  }

  /*!
  For currency positions, it may be convenient to express profit/loss value in pips.
   */
  def profitLossPips(price: BigDecimal): BigDecimal = instrument match {
    case cp: CurrencyPair => asPips(cp, (price - this.price) * primary.amount.signum)
    case _ => throw new UnsupportedOperationException("Pips operations are defined only on currency positions")
  }

  def profitLossPips(quote: Quote): Option[BigDecimal] = {
    val s = PositionSide.close(side)
    for (p <- quote(s)) yield profitLossPips(p)
  }

  def profitLossPips(market: Market): Option[BigDecimal] = for (q <- market.quote(instrument, absoluteAmount);
                                                                pl <- profitLossPips(q)) yield pl

  /*!
  Two positions can be merged, in such a way that:
  * Two positions with the same side are merged into a larger position;
  * Two positions with equal amounts and opposite sides collapse; some profit or loss appears from this operation;
  * Two positions with different amounts and opposite sides partially collapse; some profit or loss appears from this
    operation; remaining position is returned.
   */
  def merge(that: Position): (Option[Position], Money) = {

    require(this.instrument == that.instrument)

    // a, b: initial positions;
    // c: position to collapse;
    // d: remaining position;
    // e: profit/loss position (with zero primary amount)
    // f: resulting position.

    val a1 = this.primary.amount
    val a2 = this.secondary.amount
    val b1 = that.primary.amount
    val b2 = that.secondary.amount
    val c1: BigDecimal = (if (a1.signum * b1.signum == -1) a1.abs min b1.abs else Money.ZERO) * a1.signum
    val c2: BigDecimal = if (a1 == Money.ZERO) a2 else c1 * (a2 / a1)
    val d1: BigDecimal = -c1
    val d2: BigDecimal = if (b1 == Money.ZERO) b2 else d1 * (b2 / b1)
    // e1 is always zero
    val e2: BigDecimal = c2 + d2
    val sigma = if (a1.abs > b1.abs) -1 else 1
    val f1: BigDecimal = a1 + b1
    val f2: BigDecimal = if (a1.signum * b1.signum == 1) a2 + b2 else if (sigma < 0) a2 - c2 else b2 - d2

    val pos: Option[Position] = if (f1 == Money.ZERO) None
    else
      Some(Position(Monetary(f1, primary.asset), Monetary(f2, secondary.asset), None, DateTime.now(), UUID.randomUUID()))
    (pos, Money(e2, secondary.asset))
  }

  /*!
  Merging operations can be expressed as a diff value; it can be "applied" to a portfolio, producing the new
  portfolio with desired changes.
   */
  def diff(oldPosition: Option[Position]): PortfolioDiff = oldPosition match {
    // If no old position found for this instrument -> add new position
    case None => new PortfolioDiff(AddPosition(this))
    // If old position is found...
    case Some(oldP) => {
      // Merge old and new positions
      val (merged, profitLoss) = oldP merge this
      merged match {
        // If merged positions collapsed -> remove old position, add new finished deal
        case None => {
          val deal = Deal(oldP, this.price, this.timestamp, profitLoss)
          new PortfolioDiff(RemovePosition(oldP), CreateDeal(deal))
        }
        // If merging produced new position...
        case Some(remainingPosition) => {
          // If position sides were equal, it is added position -> modify existing position
          if (oldP.side == this.side)
            new PortfolioDiff(RemovePosition(oldP), AddPosition(remainingPosition))
          else {
            // Otherwise it is partial close -> modify existing position, create partial close deal
            val deal = partialCloseDeal(oldP)
            new PortfolioDiff(RemovePosition(oldP), AddPosition(remainingPosition), CreateDeal(deal))
          }
        }
      }
    }
  }

  private def partialCloseDeal(oldPosition: Position): Deal = {
    require(oldPosition.instrument == this.instrument)
    require(oldPosition.side != this.side)
    require(oldPosition.absoluteAmount != this.absoluteAmount)
    val closingAmount = (oldPosition.absoluteAmount min this.absoluteAmount) * oldPosition.primary.amount.signum
    val closingPart = Position(oldPosition.instrument, oldPosition.price,
      closingAmount, Some(oldPosition.id), oldPosition.timestamp)
    new Deal(closingPart, this.price, this.timestamp, (oldPosition merge this)._2)
  }

  override def toString = "POSITION " + instrument + " " + primary + " @ " + price
}

object Position {
  def apply(instrument: Instrument,
            price: BigDecimal,
            amount: BigDecimal,
            matching: Option[UUID] = None,
            timestamp: DateTime = DateTime.now(),
            id: UUID = UUID.randomUUID()): Position = Position(
    primary = Monetary(amount, instrument.base),
    secondary = Monetary(-amount * price, instrument.counter),
    matching = matching,
    timestamp = timestamp,
    id = id
  )
}

object PositionSide extends Enumeration {
  val Long, Short = Value

  /*!
  What quote side to use to open a position?
   */
  def open(side: PositionSide.Value): QuoteSide.Value = side match {
    case Long => QuoteSide.Ask
    case Short => QuoteSide.Bid
  }

  /*!
  What quote side to use to close a position?
   */
  def close(side: PositionSide.Value): QuoteSide.Value = side match {
    case Long => QuoteSide.Bid
    case Short => QuoteSide.Ask
  }

  /*!
  Reverse the position side.
   */
  def reverse(side: PositionSide.Value): PositionSide.Value = side match {
    case Long => PositionSide.Short
    case Short => PositionSide.Long
  }
}
