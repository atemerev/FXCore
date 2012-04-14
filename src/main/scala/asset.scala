package com.miriamlaurel.fxcore

import java.util.Currency
import java.io.Serializable

/**
 * @author Alexander Temerev
 */
trait Asset extends Serializable {
  def code: String
  override def toString = code
}

case class CurrencyAsset(currency: Currency) extends Asset {

  def code = currency.getCurrencyCode
}

object CurrencyAsset {

  val cache = Map[String, CurrencyAsset]()

  def apply(code: String): CurrencyAsset = {
    if (cache.get(code).isEmpty) {
      val asset = new CurrencyAsset(Currency.getInstance(code))
      asset
    } else {
      cache.get(code).get
    }
  }
}
