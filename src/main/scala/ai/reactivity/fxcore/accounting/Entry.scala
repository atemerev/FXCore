package ai.reactivity.fxcore.accounting

import ai.reactivity.fxcore.asset.AssetClass
import ai.reactivity.fxcore.market.Market
import ai.reactivity.fxcore.{Money, Timestamp}

trait Entry extends Timestamp {
  def amount: Money

  def convert(to: AssetClass, market: Market): Option[Entry]
}
