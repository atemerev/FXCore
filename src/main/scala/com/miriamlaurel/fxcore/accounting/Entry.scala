package com.miriamlaurel.fxcore.accounting

import com.miriamlaurel.fxcore.{Money, Timestamp}
import com.miriamlaurel.fxcore.market.Market
import com.miriamlaurel.fxcore.asset.AssetClass

trait Entry extends Timestamp {
  def amount: Money
  def convert(to: AssetClass, market: Market): Option[Entry]
}
