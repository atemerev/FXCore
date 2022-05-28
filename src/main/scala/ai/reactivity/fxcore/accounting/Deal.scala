package ai.reactivity.fxcore.accounting

import ai.reactivity.fxcore.{Money, SafeDouble}
import ai.reactivity.fxcore.asset.AssetClass
import ai.reactivity.fxcore.market.Market
import ai.reactivity.fxcore.portfolio.{Position, PositionSide}

case class Deal(position: Position, closePrice: SafeDouble, override val timestamp: Long, override val amount: Money) extends Entry {
  override def convert(to: AssetClass, market: Market): Option[Entry] = for {
    converted <- market.convert(amount, to, PositionSide.close(position.side), position.absoluteAmount)
  } yield copy(amount = converted)

  override def toString = "Position %s closed at %f".format(position, closePrice.toDouble)
}
