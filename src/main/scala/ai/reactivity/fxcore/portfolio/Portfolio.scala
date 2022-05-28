package ai.reactivity.fxcore.portfolio

import ai.reactivity.fxcore.{Money, SafeDouble}
import ai.reactivity.fxcore.asset.AssetClass
import ai.reactivity.fxcore.instrument.Instrument
import ai.reactivity.fxcore.market.Market

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
  def amount(instrument: Instrument): SafeDouble = this.positions(instrument).map(_.absoluteAmount).foldLeft(SafeDouble(0))(_ + _)

  /*!
  Non-absolute amount value, collapsing all positions.
   */
  def total(instrument: Instrument): SafeDouble = this.positions(instrument).map(_.primary.amount).foldLeft(SafeDouble(0))(_ + _)

  /*!
  Total profit/loss for all positions in portfolio. Since it can be calculated in any asset (currency), a
  Market instance is required.
   */
  def profitLoss(asset: AssetClass, market: Market): Option[Money] = {
    val plValues = this.positions.map(_.profitLossIn(asset, market))
    if (plValues.exists(_.isEmpty)) None
    else
      Some(Money((for (i <- plValues; v <- i) yield v.amount).foldLeft(SafeDouble(0))(_ + _), asset))
  }

  /*!
  By default, total profit/loss expressed in market's pivot asset.
   */
  def profitLoss(market: Market): Option[Money] = profitLoss(market.pivot, market)
}
