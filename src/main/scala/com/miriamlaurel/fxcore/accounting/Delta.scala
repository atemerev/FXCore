package com.miriamlaurel.fxcore.accounting

import org.joda.time.DateTime
import com.miriamlaurel.fxcore.Money
import com.miriamlaurel.fxcore.asset.AssetClass
import com.miriamlaurel.fxcore.market.{QuoteSide, Market}

case class Delta(override val timestamp: DateTime, override val amount: Money) extends Entry {
  override def convert(to: AssetClass, market: Market): Option[Delta] = for {
    converted <- market.convert(amount, to, QuoteSide.Ask, amount.amount)
  } yield copy()
}