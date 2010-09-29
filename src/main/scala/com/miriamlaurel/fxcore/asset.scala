package com.miriamlaurel.fxcore

import java.util.Currency

/**
 * @author Alexander Temerev
 */
abstract class Asset {
  def code: String
}

class CurrencyAsset(val currency: Currency) extends Asset {

  def code = currency.getCurrencyCode

  override def equals(obj: Any) = obj.isInstanceOf[CurrencyAsset] &&
          obj.asInstanceOf[CurrencyAsset].currency == currency

  override def hashCode = currency.hashCode

  override def toString = currency.toString
}

object CurrencyAsset {

  val cache = Map[String, CurrencyAsset]()

  def apply(code: String): CurrencyAsset = {
    if (cache.get(code).isEmpty) {
      val asset = new CurrencyAsset(Currency.getInstance(code))
      return asset
    } else {
      return cache.get(code).get
    }
  }
}
