package com.miriamlaurel.fxcore.accounting

import java.util.UUID
import com.miriamlaurel.fxcore.{Identity, Zilch, Money}
import com.miriamlaurel.fxcore.portfolio._
import com.miriamlaurel.fxcore.asset.{Currency, AssetClass}
import com.miriamlaurel.fxcore.market.Market

case class Account (
                     portfolio: Portfolio,
                     asset: AssetClass = Currency("USD"),
                     initialBalance: Money = Zilch,
                     deals: List[Deal] = List[Deal](),
                     diff: Option[PortfolioDiff] = None,
                     override val id: UUID = UUID.randomUUID()) extends Identity {

  lazy val closeProfitLoss: Money = deals.map(_.profitLoss).foldRight(Zilch: Money)(_ + _)

  def <<(position: Position, market: Market): Option[Account] = {
    val (newPortfolio, diff) = portfolio << position
    val deals: List[Deal] = diff.actions.collect{case CreateDeal(deal) => deal}
    for (convertedDiff <- convertDiff(diff, market)) yield new Account(newPortfolio, asset, initialBalance,
      this.deals ++ deals, Some(convertedDiff), id)
  }

  def applyDiff(diff: PortfolioDiff) = new Account(portfolio.apply(diff), asset, initialBalance, deals, Some(diff), id)

  def total(market: Market): Option[Money] = for {
    openPl <- portfolio.profitLoss(asset, market)
  } yield initialBalance + openPl + closeProfitLoss

  private def convertDiff(diff: PortfolioDiff, market: Market): Option[PortfolioDiff] = {
    val newActions: List[PortfolioAction] = diff.actions.flatMap {
      case CreateDeal(deal) => for (d <- convertDeal(deal, market)) yield CreateDeal(d)
      case x: PortfolioAction => Some(x)
    }
    // a hack, but I don't know enough functional kung-fu to do otherwise...
    if (diff.actions.size == newActions.size) Some(new PortfolioDiff(newActions: _*)) else None
  }

  private def convertDeal(deal: Deal, market: Market): Option[Deal] = {
    val rawPl = deal.profitLoss
    val closeSide = PositionSide.close(deal.position.side)
    for {
      converted <- market.convert(rawPl, this.asset, closeSide, deal.position.absoluteAmount)
    } yield Deal(deal.position, deal.closePrice, deal.timestamp, converted)
  }
}