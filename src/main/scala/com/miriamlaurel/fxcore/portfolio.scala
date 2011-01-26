package com.miriamlaurel.fxcore

import com.miriamlaurel.fxcore.pipscaler._
import scala.math._
import com.miriamlaurel.fxcore.numbers._
import java.io.Serializable
import java.util.{Date, UUID}

/**
 * @author Alexander Temerev
 */
class Position(val primary: Monetary,
               val secondary: Monetary,
               val matching: Option[UUID] = None,
               override val timestamp:
               Date = new Date(),
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

  require(primary.amount.signum != secondary.amount.signum)

  lazy val instrument = Instrument(primary.asset, secondary.asset)
  lazy val price: Decimal = (secondary.amount / primary.amount).abs
  lazy val side = if (primary.amount > 0) PositionSide.Long else PositionSide.Short
  lazy val amount: Decimal = primary.amount.abs

  def profitLoss(newPrice: Decimal): Money = {
    Money((newPrice - price) * primary.amount, secondary.asset)
  }

  def profitLoss(q: Quote): Option[Money] =
    for (price <- if (side == PositionSide.Long) q.bid else q.ask) yield profitLoss(price)

  def profitLossIn(asset: Asset, market: Market): Option[Money] = {
    for (q <- market.quote(instrument, amount);
         raw <- profitLoss(q);
         side = if (raw.amount >= 0) OfferSide.Bid else OfferSide.Ask;
         m <- market.convert(raw, asset, side, amount)) yield m
  }

  def close(market: Market): Option[Position] = {
    for (quote <- market.quote(instrument, amount);
         price <- if (side == PositionSide.Long) quote.bid else quote.ask)
    yield new Position(instrument, price, -primary.amount)
  }

  def profitLossPips(price: Decimal): Decimal = instrument match {
    case cp: CurrencyPair => asPips(cp, (price - this.price) * primary.amount.signum)
    case _ => throw new UnsupportedOperationException("Pips operations are defined only on currency positions")
  }

  def profitLossPips(market: Market): Option[Decimal] = for (q <- market.quote(instrument, amount);
                                                             s = PositionSide.close(side);
                                                             p <- q(s)) yield profitLossPips(p)

  def merge(that: Position): (Option[Position], Money) = {

    require(this.instrument == that.instrument)

    // This is highly magical code ported from FI Java implementation once written by me on a sheet of
    // paper and never challenged since then.

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

class Deal(val position: Position, val closePrice: Decimal, val closeTimestamp: Date, val profitLoss: Money)

object PositionSide extends Enumeration {
  val Long, Short = Value

  def open(side: PositionSide.Value): OfferSide.Value = side match {
    case Long => OfferSide.Ask
    case Short => OfferSide.Bid
  }

  def close(side: PositionSide.Value): OfferSide.Value = side match {
    case Long => OfferSide.Bid
    case Short => OfferSide.Ask
  }

  def reverse(side: PositionSide.Value): PositionSide.Value = side match {
    case Long => Short
    case Short => Long
  }
}

trait Portfolio {

  def apply(diff: PortfolioDiff): Portfolio

  def positions: Iterable[Position]

  def positions(instrument: Instrument): Iterable[Position]

  def <<(position: Position): (Portfolio, PortfolioDiff)

  def amount(instrument: Instrument): Decimal = this.positions(instrument).map(_.amount).reduceLeft(_ + _)

  def profitLoss(asset: Asset, market: Market): Option[Money] = {
    // I believe this can be done better
    val plValues = this.positions.map(_.profitLossIn(asset, market))
    if (plValues.exists(_.isEmpty)) None
    else
      Some(Money((for (i <- plValues; v <- i) yield v.amount).foldLeft(Decimal(0))(_ + _), asset))
  }

  def profitLoss(market: Market): Option[Money] = profitLoss(market.pivot, market)
}

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

  def position(instrument: Instrument): Option[Position] = map.get(instrument)

  def size = map.size
}

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

sealed abstract class PortfolioAction(val appliedPosition: Position)

case class AddPosition(position: Position) extends PortfolioAction(position)

case class RemovePosition(position: Position) extends PortfolioAction(position)

case class CreateDeal(deal: Deal) extends PortfolioAction(deal.position)

case class MergePositions(instrument: Instrument, merged: Set[Position], result: Option[Position], adjustment: Money) extends PortfolioAction(null)

class PortfolioDiff(acs: PortfolioAction*) {
  val actions = acs.toList

  def +(action: PortfolioAction) = new PortfolioDiff((action :: this.actions): _*)
}

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

  /*
    def merge(uuids: Set[UUID], market: Market): Option[Account] = {
      val toMerge = portfolio.positions.filter(position => uuids.contains(position.uuid))
      require(toMerge.map(_.instrument).toSet.size == 1, "Can't merge positions with different instruments")
      val instrument = toMerge.head.instrument
      var merged: Option[Position] = None
      var adjustment: Money = Zilch
      try {
        toMerge.foreach(position => {
          merged match {
            case None => merged = Some(position)
            case Some(pos) => {
              val (p, m) = pos.merge(position)
              merged = p
              val converted = market.convert(m, this.asset, PositionSide.close(position.side), position.amount)
              converted match {
                case Some(amount) => adjustment = adjustment + amount
                case None => throw new ConversionException
              }
            }
          }
        })
        var newMap = (portfolio.details(merged.instrument) -- uuids)
        merged match {
          case Some(position) => newMap = newMap + (merged.uuid -> merged)
          case None => // do nothing
        }
        val newDetails = portfolio.details + (instrument -> newMap)
        val newPortfolio = new NonStrictPortfolio(newDetails)
        val diff = new PortfolioDiff(MergePositions(instrument, toMerge, merged, adjustment))
        Some(new Account(newPortfolio, this.asset, balance + adjustment, diff, this.scale))
      }
      catch {
        case ConversionException => None
      }
    }
  */

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

class ConversionException extends Exception