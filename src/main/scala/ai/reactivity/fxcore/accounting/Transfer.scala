package ai.reactivity.fxcore.accounting

import java.time.Instant

import ai.reactivity.fxcore.Money
import ai.reactivity.fxcore.asset.AssetClass
import ai.reactivity.fxcore.market.{Market, QuoteSide}

case class Transfer(override val timestamp: Long, override val amount: Money, reference: String, settled: Boolean = false) extends Entry {
  override def convert(to: AssetClass, market: Market): Option[Transfer] = for {
    converted <- market.convert(amount, to, QuoteSide.Ask, amount.amount)
  } yield copy()

  override def toString = "Transferred %s".format(amount)
}
