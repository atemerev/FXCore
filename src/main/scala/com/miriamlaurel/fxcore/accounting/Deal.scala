package com.miriamlaurel.fxcore.accounting

import com.miriamlaurel.fxcore.{Money, SafeDouble}
import com.miriamlaurel.fxcore.asset.AssetClass
import com.miriamlaurel.fxcore.market.Market
import com.miriamlaurel.fxcore.portfolio.{Position, PositionSide}

case class Deal(position: Position, closePrice: SafeDouble, override val timestamp: Long, override val amount: Money) extends Entry {
  override def convert(to: AssetClass, market: Market): Option[Entry] = for {
    converted <- market.convert(amount, to, PositionSide.close(position.side), position.absoluteAmount)
  } yield copy(amount = converted)

  override def toString = "Position %s closed at %f".format(position, closePrice.toDouble)
}
