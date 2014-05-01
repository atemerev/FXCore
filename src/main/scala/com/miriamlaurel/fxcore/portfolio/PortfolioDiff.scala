package com.miriamlaurel.fxcore.portfolio

import com.miriamlaurel.fxcore.Money
import com.miriamlaurel.fxcore.instrument.Instrument
import com.miriamlaurel.fxcore.accounting.Deal

case class PortfolioDiff(acs: PortfolioAction*) {

  val actions = acs.toList

  def +(action: PortfolioAction) = PortfolioDiff(action :: this.actions: _*)
}

sealed abstract class PortfolioAction(val appliedPosition: Position)

case class AddPosition(position: Position) extends PortfolioAction(position)

case class RemovePosition(position: Position) extends PortfolioAction(position)

case class CreateDeal(deal: Deal) extends PortfolioAction(deal.position)

case class MergePositions(instrument: Instrument, merged: Set[Position], result: Option[Position], adjustment: Money) extends PortfolioAction(null)
