package com.miriamlaurel.fxcore

import com.miriamlaurel.fxcore.pipscaler._
import com.miriamlaurel.fxcore.numbers._
import java.util.{Date, UUID}

/**
 * @author: Alexander Temerev; temerev@miriamlaurel.com
 */

/*!# Position
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
class Position(val primary: Monetary,
               val secondary: Monetary,
               val matching: Option[UUID] = None,
               override val timestamp: Date = new Date(),
               override val uuid: UUID = UUID.randomUUID)
  extends Entity with TimeEvent {

  def this(instrument: Instrument, price: Decimal, amount: Decimal) =
    this (Monetary(amount, instrument.primary), Monetary(-amount * price, instrument.secondary))

  def this(instrument: Instrument, price: Decimal, amount: Decimal, matching: Option[UUID]) =
    this (Monetary(amount, instrument.primary), Monetary(-amount * price, instrument.secondary), matching)

  def this(instrument: Instrument, price: Decimal, amount: Decimal, matching: Option[UUID], timestamp: Date) =
    this (Monetary(amount, instrument.primary), Monetary(-amount * price, instrument.secondary), matching, timestamp)

  def this(instrument: Instrument, price: Decimal, amount: Decimal, matching: Option[UUID], timestamp: Date, uuid: UUID) =
    this (Monetary(amount, instrument.primary), Monetary(-amount * price, instrument.secondary),
      matching, timestamp, uuid)

  /*!
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
  lazy val price: Decimal = (secondary.amount / primary.amount).abs

  /*!
  Position can be either long (primary amount is positive) or short (negative). Short positions may be handled
  differently dependent on asset class; in forex, long and short positions are usually quite symmetrical.
   */
  lazy val side = if (primary.amount > 0) PositionSide.Long else PositionSide.Short

  /*!
  Position amount is the absolute value of primary asset amount, i.e. it is always positive. If amount sign
  matters (it usually does), one should use .primary.amount
   */
  lazy val amount: Decimal = primary.amount.abs

  /*!
  Create a reversed position with new price and matching UUID. This can be used efficiently to close a position.
   */
  def close(newPrice: Decimal): Position = new Position(instrument, price, -primary.amount, Some(uuid))

  /*!
  Position's profit or loss for any price level can be easily calculated, provided it's expressed in position's
  secondary asset units.
   */
  def profitLoss(newPrice: Decimal): Money = {
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
  def profitLossIn(asset: Asset, market: Market): Option[Money] = {
    for (q <- market.quote(instrument, amount);
         raw <- profitLoss(q);
         side = if (raw.amount >= 0) OfferSide.Bid else OfferSide.Ask;
         m <- market.convert(raw, asset, side, amount)) yield m
  }

  /*!
  For currency positions, it may be convenient to express profit/loss value in pips.
   */
  def profitLossPips(price: Decimal): Decimal = instrument match {
    case cp: CurrencyPair => asPips(cp, (price - this.price) * primary.amount.signum)
    case _ => throw new UnsupportedOperationException("Pips operations are defined only on currency positions")
  }

  def profitLossPips(market: Market): Option[Decimal] = for (q <- market.quote(instrument, amount);
                                                             s = PositionSide.close(side);
                                                             p <- q(s)) yield profitLossPips(p)

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
    val c1: Decimal = (if (a1.signum * b1.signum == -1) a1.abs min b1.abs else Decimal(0)) * a1.signum
    val c2: Decimal = if (a1 == 0) a2 else c1 * (a2 / a1)
    val d1: Decimal = -c1
    val d2: Decimal = if (b1 == 0) b2 else d1 * (b2 / b1)
    // e1 is always zero
    val e2: Decimal = c2 + d2
    val sigma = if (a1.abs > b1.abs) -1 else 1
    val f1: Decimal = a1 + b1
    val f2: Decimal = if (a1.signum * b1.signum == 1) a2 + b2 else if (sigma < 0) a2 - c2 else b2 - d2

    val pos: Option[Position] = if (f1 == 0) None
    else
      Some(new Position(Monetary(f1, primary.asset), Monetary(f2, secondary.asset)))
    return (pos, Money(e2, secondary.asset))
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
          val deal = new Deal(oldP, this.price, this.timestamp, profitLoss)
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
    require(oldPosition.amount != this.amount)
    val closingAmount = (oldPosition.amount min this.amount) * oldPosition.primary.amount.signum
    val closingPart = new Position(oldPosition.instrument, oldPosition.price,
      closingAmount, Some(oldPosition.uuid), oldPosition.timestamp)
    new Deal(closingPart, this.price, this.timestamp, (oldPosition merge this)._2)
  }

  override def toString = "POSITION " + instrument + " " + primary + " @ " + price
}

/*!
A deal is a closed position, with fixed ("realized") profit/loss.
 */
class Deal(val position: Position, val closePrice: Decimal, val closeTimestamp: Date, val profitLoss: Money)


/*!
We can have long and short positions...
 */
object PositionSide extends Enumeration {
  val Long, Short = Value

  /*!
  What quote side to use to open a position?
   */
  def open(side: PositionSide.Value): OfferSide.Value = side match {
    case Long => OfferSide.Ask
    case Short => OfferSide.Bid
  }

  /*!
  What quote side to use to close a position?
   */
  def close(side: PositionSide.Value): OfferSide.Value = side match {
    case Long => OfferSide.Bid
    case Short => OfferSide.Ask
  }

  /*!
  Reverse the position side.
   */
  def reverse(side: PositionSide.Value): PositionSide.Value = side match {
    case Long => PositionSide.Short
    case Short => PositionSide.Long
  }
}

/*!#Portfolio
There can be many different types of portfolios, but all they have common properties summarized by this trait:
 */
trait Portfolio {

  /*!
  A diff value may be applied to this portfolio, giving new (changed) portfolio;
   */
  def apply(diff: PortfolioDiff): Portfolio

  /*!
  List of all portfolio positions;
   */
  def positions: Iterable[Position]

  /*!
  List of portfolio positions by instrument. In "strict" portfolio implementation, this list always contain
  zero or one positions.
   */
  def positions(instrument: Instrument): Iterable[Position]

  /*!
  "Accept" operation takes a position and "applies" it to portfolio, producing new portfolio and diff value --
  set of changes.
   */
  def <<(position: Position): (Portfolio, PortfolioDiff)

  /*!
  Total amount of all positions in portfolio (expressed in their primary asset). Not that useful beyond
  forex portfolios, though...
   */
  def amount(instrument: Instrument): Decimal = this.positions(instrument).map(_.amount).foldLeft(Decimal(0))(_ + _)

  /*!
  Non-absolute amount value, collapsing all positions.
   */
  def total(instrument: Instrument): Decimal = this.positions(instrument).map(_.primary.amount).foldLeft(Decimal(0))(_ + _)

  /*!
  Total profit/loss for all positions in portfolio. Since it can be calculated in any asset (currency), a
  Market instance is required.
   */
  def profitLoss(asset: Asset, market: Market): Option[Money] = {
    val plValues = this.positions.map(_.profitLossIn(asset, market))
    if (plValues.exists(_.isEmpty)) None
    else
      Some(Money((for (i <- plValues; v <- i) yield v.amount).foldLeft(Decimal(0))(_ + _), asset))
  }

  /*!
  By default, total profit/loss expressed in market's pivot asset.
   */
  def profitLoss(market: Market): Option[Money] = profitLoss(market.pivot, market)
}

/*!##StrictPortfolio
A "strict" portfolio doesn't store different positions for the same instrument; only aggregate positions are
stored. This behavior is consistent with position handling by most ECNs.

Even more "stricter" portfolio implementation is possible for forex market, which automatically collapses positions
by currencies in its instruments' currency pairs; for example, EUR/CHF can be splitted to EUR/USD and USD/CHF
positions. This "stricter" portfolio is normally used by banks, but we don't need it at the moment.
 */
class StrictPortfolio protected(val map: Map[Instrument, Position]) extends Portfolio {
  def this() = this (Map())

  lazy val positions = map.values

  def apply(diff: PortfolioDiff): StrictPortfolio = {
    var newMap = map
    for (action <- diff.actions) {
      action match {
        case AddPosition(p) => {
          require(!(newMap contains p.instrument))
          newMap = newMap + (p.instrument -> p)
        }
        case RemovePosition(p) => {
          require(newMap contains p.instrument)
          newMap = newMap - p.instrument
        }
        case _ => // Ignore
      }
    }
    new StrictPortfolio(newMap)
  }

  def <<(newPosition: Position): (StrictPortfolio, PortfolioDiff) = {
    val oldPosition = map.get(newPosition.instrument)
    val diff = newPosition diff oldPosition
    (this(diff), diff)
  }

  def positions(instrument: Instrument): Iterable[Position] = position(instrument) match {
    case Some(pos) => List(pos)
    case None => List()
  }

  /*!
  There can be exactly 0 or 1 positions for each instrument...
   */
  def position(instrument: Instrument): Option[Position] = map.get(instrument)

  def size = map.size
}

/*!##NonStrictPortfolio
A "non-strict" portfolio allows multiple positions for the same instrument. This behavior is consistent with
position handling by most market makers.
 */
class NonStrictPortfolio protected(val details: Map[Instrument, Map[UUID, Position]]) extends Portfolio {

  def this() = this (Map())

  override lazy val positions: Iterable[Position] = details.flatMap(_._2.values)

  override def positions(instrument: Instrument): Iterable[Position] =
    details.getOrElse(instrument, Map[UUID, Position]()).values

  override def apply(diff: PortfolioDiff): NonStrictPortfolio = {
    var newDetails = details
    for (action <- diff.actions) {
      action match {
        case AddPosition(p) => {
          val byInstrument = newDetails.getOrElse(p.instrument, Map[UUID, Position]())
          require(!(byInstrument contains p.uuid))
          newDetails = newDetails + (p.instrument -> (byInstrument + (p.uuid -> p)))
        }
        case RemovePosition(p) => {
          val byInstrument = newDetails.getOrElse(p.instrument, Map[UUID, Position]())
          require(byInstrument contains p.uuid)
          newDetails = newDetails + (p.instrument -> (byInstrument - p.uuid))
        }
        case _ => // Ignore
      }
    }
    new NonStrictPortfolio(newDetails)
  }

  /*!
  We can merge positions with same instrument into one. This method return new non-strict portfolio (with
  merged positions) and a diff value containing all changes have been made. Merge operation can produce ("realize")
  profit or loss, which is stored in a diff value as an "adjustment".
   */
  def mergePositions(uuids: Set[UUID]): (NonStrictPortfolio, PortfolioDiff) = {
    val toMerge = positions.filter(position => uuids.contains(position.uuid))
    if (toMerge.size == 0) (this, new PortfolioDiff())
    else {
      require(toMerge.map(_.instrument).toSet.size == 1, "Can't merge positions with different instruments")
      val instrument = toMerge.head.instrument
      var merged: Option[Position] = None
      var adjustment: Money = Zilch
      toMerge.foreach(position => {
        merged match {
          case None => merged = Some(position)
          case Some(pos) => {
            val (p, m) = pos.merge(position)
            merged = p
            adjustment = adjustment + m
          }
        }
      })
      var newMap = (details(instrument) -- uuids)
      merged match {
        case Some(position) => newMap = newMap + (position.uuid -> position)
        case None => // do nothing
      }
      val newDetails = details + (instrument -> newMap)
      val newPortfolio = new NonStrictPortfolio(newDetails)
      val diff = new PortfolioDiff(MergePositions(instrument, toMerge.toSet, merged, adjustment))
      (newPortfolio, diff)
    }
  }

  override def <<(newPosition: Position): (NonStrictPortfolio, PortfolioDiff) = {
    val oldPosition = newPosition.matching match {
      case None => None
      case Some(uuid) => for (byInstrument <- details.get(newPosition.instrument);
                              p <- byInstrument.get(uuid)) yield p
    }
    val diff = newPosition diff oldPosition
    (this(diff), diff)
  }
}

/*!#PortfolioAction
Portfolio action is a "change" that can be applied to portfolio giving new portfolio (or produced by applying a
position to portfolio). Haskell aficionados will probably recognize a State monad; actual implementation of monadic
properties might get done in the near future.
 */
sealed abstract class PortfolioAction(val appliedPosition: Position)

case class AddPosition(position: Position) extends PortfolioAction(position)

case class RemovePosition(position: Position) extends PortfolioAction(position)

case class CreateDeal(deal: Deal) extends PortfolioAction(deal.position)

case class MergePositions(instrument: Instrument, merged: Set[Position], result: Option[Position], adjustment: Money) extends PortfolioAction(null)

/*!#PortfolioDiff
A diff value is just a list of portfolio actions that can be applied sequentially.
 */
class PortfolioDiff(acs: PortfolioAction*) {
  val actions = acs.toList

  def +(action: PortfolioAction) = new PortfolioDiff((action :: this.actions): _*)
}

class ConversionException extends Exception

class Account(
               val portfolio: Portfolio,
               val asset: Asset = CurrencyAsset("USD"),
               val balance: Money = Zilch,
               val diff: Option[PortfolioDiff] = None,
               val scale: Int = 2) {

  def <<(position: Position, market: Market): Option[Account] = {
    val (newPortfolio, diff) = portfolio << position
    val deals = diff.actions.filter(_.isInstanceOf[CreateDeal])
    val profitLoss = if (deals.size > 0) {
      val deal = deals(0).asInstanceOf[CreateDeal].deal
      deal.profitLoss
    } else Zilch
    val closeSide = position.side match {
      case PositionSide.Long => OfferSide.Bid
      case PositionSide.Short => OfferSide.Ask
    }
    for (converted <- market.convert(profitLoss, asset, closeSide, position.amount);
         newBalance = (balance + converted).setScale(scale);
         convertedDiff = convertDiff(diff, market))
    yield new Account(newPortfolio, asset, newBalance, Some(convertedDiff), scale)
  }

  private def convertDiff(diff: PortfolioDiff, market: Market): PortfolioDiff = {
    val newActions = diff.actions.map(_ match {
      case CreateDeal(deal) => CreateDeal(convertDeal(deal, market))
      case x => x
    })
    new PortfolioDiff(newActions: _*)
  }

  private def convertDeal(deal: Deal, market: Market): Deal = {
    val rawPl = deal.profitLoss
    val closeSide = PositionSide.close(deal.position.side)
    val converted = market.convert(rawPl, this.asset, closeSide, deal.position.amount)
    new Deal(deal.position, deal.closePrice, deal.closeTimestamp, converted.get)
  }
}