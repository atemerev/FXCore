package com.miriamlaurel.fxcore.accounting

import java.time.Instant

import com.miriamlaurel.fxcore.Money
import com.miriamlaurel.fxcore.asset.AssetClass
import com.miriamlaurel.fxcore.market.Market
import com.miriamlaurel.fxcore.portfolio.{Position, PositionSide}

case class Deal(position: Position, closePrice: BigDecimal, override val timestamp: Instant, override val amount: Money) extends Entry {
  override def convert(to: AssetClass, market: Market): Option[Entry] = for {
    converted <- market.convert(amount, to, PositionSide.close(position.side), position.absoluteAmount)
  } yield copy(amount = converted)

  override def toString = "Position %s closed at %f".format(position, closePrice)
}
