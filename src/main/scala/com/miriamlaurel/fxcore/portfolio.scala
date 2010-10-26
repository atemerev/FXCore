package com.miriamlaurel.fxcore

import com.miriamlaurel.fxcore.pipscaler._
import scala.math._
import java.util.UUID
import com.miriamlaurel.fxcore.numbers._
import java.io.Serializable


/**
 * @author Alexander Temerev
 */
class Position(val primary: Monetary, val secondary: Monetary, matching: UUID = null) extends Entity {
  def this(instrument: Instrument, price: Decimal, amount: Decimal) =
    this (Monetary(amount, instrument.primary), Monetary(-amount * price, instrument.secondary))

  val matchUuid: Option[UUID] = if (matching != null) Some(matching) else None

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
         s = if (raw.amount >= 0) OfferSide.Bid else OfferSide.Ask;
         m <- market.convert(raw, asset, s, amount)) yield m
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


/*

    public Position merge(Position newPosition) {

        // a, b: initial positions;
        // c: position to collapse;
        // d: remaining position;
        // e: profit/loss position (with zero primary amount)
        // f: resulting position.

        Monetary a1 = this.primary.getValue();
        Monetary a2 = this.secondary.getValue();
        Monetary b1 = newPosition.primary.getValue();
        Monetary b2 = newPosition.secondary.getValue();

        Monetary c1 = a1.signum() * b1.signum() == -1 ?
                Monetary.min(a1.abs(), b1.abs()) : Monetary.ZERO;
        c1 = a1.signum() == -1 ? c1.negate() : c1; // faster than direct multiplying
        Monetary c2 = a1.isZero() ? a2 : c1.multiply((a2.divide(a1)));

        Monetary d1 = c1.negate();
        Monetary d2 = b1.isZero() ? b2 : d1.multiply(b2.divide(b1));

        // e1 is always zero
        Monetary e2 = c2.add(d2);

        Monetary sigma = a1.abs().compareTo(b1.abs()) > 0 ? Monetary.ONE.negate() : Monetary.ONE;
        Monetary f1 = a1.add(b1);
//        Monetary f2 = c1.isZero() ? a2.add(b2) : b2.subtract(d2.multiply(sigma));
        Monetary f2 = a1.signum() * b1.signum() == 1 ? a2.add(b2) :
                sigma.isNegative() ? a2.subtract(c2) : b2.subtract(d2);

        Position position = new Position(
                new Money(f1, primary.getAsset()),
                new Money(f2, secondary.getAsset()));
        position.setProfitLoss(e2);
        return position;
    }
*/

  def merge(that: Position): Pair[Option[Position], Money] = {

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

    val pos: Option[Position] = if (f1 == 0) None else
      Some(new Position(Monetary(f1, primary.asset), Monetary(f2, secondary.asset)))
    return (pos, Money(e2, secondary.asset))
  }

  override def toString = "POSITION " + instrument + " " + primary + " @ " + price
}

object PositionSide extends Enumeration {
  val Long, Short = Value
}

abstract class Portfolio extends Serializable {
  def <<(position: Position): Pair[StrictPortfolio, Money]

  def amount(instrument: Instrument): Decimal

  def profitLossIn(asset: Asset, market: Market): Option[Money]

  def profitLoss(market: Market) = profitLossIn(market.pivot, market)
}

class StrictPortfolio(private val map: Map[Instrument, Position] = Map()) extends Portfolio {

  val positions = map.values

  def <<(newPosition: Position): Pair[StrictPortfolio, Money] = {
    val oldPosition = map.get(newPosition.instrument)
    val (pos, profitLoss) = oldPosition match {
      case Some(p) => p merge newPosition
      case None => (Some(newPosition), Money(0, newPosition.secondary.asset))
    }
    val newMap = pos match {
      case Some(position) => map + (position.instrument -> position)
      case None => map - newPosition.instrument
    }
    (new StrictPortfolio(newMap), profitLoss)
  }

  def position(instrument: Instrument): Option[Position] = map.get(instrument)

  def amount(instrument: Instrument): Decimal = map.get(instrument) match {
    case None => 0
    case Some(position) => position.primary.amount
  }

  def profitLossIn(asset: Asset, market: Market): Option[Money] = {
    // I believe this can be done better
    val plValues = positions.map(_.profitLossIn(asset, market))
    if (plValues.exists(_.isEmpty)) None else
      Some(Money((for (i <- plValues; v <- i) yield v.amount).foldLeft(Decimal(0))(_ + _), asset))
  }

  def size = map.size
}

