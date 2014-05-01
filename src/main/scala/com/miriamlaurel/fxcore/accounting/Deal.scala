package com.miriamlaurel.fxcore.accounting

import org.joda.time.DateTime
import com.miriamlaurel.fxcore.Money
import com.miriamlaurel.fxcore.portfolio.{PositionSide, Position}
import com.miriamlaurel.fxcore.market.Market
import com.miriamlaurel.fxcore.asset.AssetClass

case class Deal(position: Position, closePrice: BigDecimal, override val timestamp: DateTime, override val amount: Money) extends Entry {
  override def convert(to: AssetClass, market: Market): Option[Entry] = for {
      converted <- market.convert(amount, to, PositionSide.close(position.side), position.absoluteAmount)
  } yield copy(amount = converted)
}