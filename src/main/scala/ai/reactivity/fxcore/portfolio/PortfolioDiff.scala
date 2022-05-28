package ai.reactivity.fxcore.portfolio

import ai.reactivity.fxcore.Money
import ai.reactivity.fxcore.accounting.Deal
import ai.reactivity.fxcore.instrument.Instrument

case class PortfolioDiff(acs: PortfolioAction*) {

  val actions = acs.toList

  def +(action: PortfolioAction) = PortfolioDiff(action :: this.actions: _*)
}

sealed abstract class PortfolioAction(val appliedPosition: Position)

case class AddPosition(position: Position) extends PortfolioAction(position)

case class RemovePosition(position: Position) extends PortfolioAction(position)

case class CreateDeal(deal: Deal) extends PortfolioAction(deal.position)

case class MergePositions(instrument: Instrument, merged: Set[Position], result: Option[Position], adjustment: Money) extends PortfolioAction(null)
