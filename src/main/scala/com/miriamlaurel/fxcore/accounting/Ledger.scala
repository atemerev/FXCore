package com.miriamlaurel.fxcore.accounting

import com.miriamlaurel.fxcore._
import com.miriamlaurel.fxcore.asset.{AssetClass, Currency}
import com.miriamlaurel.fxcore.market.Market

case class Ledger(accountingAsset: AssetClass = Currency("USD"), entries: Seq[Entry] = Seq()) {

  lazy val balance: Money = entries.map(_.amount).foldRight(Zilch: Money)(_ + _)

  def +(entry: Entry) = Ledger(accountingAsset, entries :+ entry)

  def putConverted(entry: Entry, market: Market): Option[Ledger] = for {
    converted <- entry.convert(accountingAsset, market)
  } yield this + converted
}