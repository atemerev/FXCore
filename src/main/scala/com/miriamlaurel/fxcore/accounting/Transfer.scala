package com.miriamlaurel.fxcore.accounting

import java.time.Instant

import com.miriamlaurel.fxcore.Money
import com.miriamlaurel.fxcore.asset.AssetClass
import com.miriamlaurel.fxcore.market.{Market, QuoteSide}

case class Transfer(override val timestamp: Instant, override val amount: Money, reference: String, settled: Boolean = false) extends Entry {
  override def convert(to: AssetClass, market: Market): Option[Transfer] = for {
    converted <- market.convert(amount, to, QuoteSide.Ask, amount.amount)
  } yield copy()

  override def toString = "Transferred %s".format(amount)
}
