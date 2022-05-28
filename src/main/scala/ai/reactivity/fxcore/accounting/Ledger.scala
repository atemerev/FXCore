package ai.reactivity.fxcore.accounting

import ai.reactivity.fxcore._
import ai.reactivity.fxcore.asset.{AssetClass, Currency}
import ai.reactivity.fxcore.market.Market

case class Ledger(accountingAsset: AssetClass = Currency("USD"), entries: Seq[Entry] = Seq()) {

  lazy val balance: Money = entries.map(_.amount).foldRight(Zilch: Money)(_ + _)

  def +(entry: Entry) = Ledger(accountingAsset, entries :+ entry)

  def putConverted(entry: Entry, market: Market): Option[Ledger] = for {
    converted <- entry.convert(accountingAsset, market)
  } yield this + converted
}