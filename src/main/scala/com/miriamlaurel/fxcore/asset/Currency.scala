package com.miriamlaurel.fxcore.asset

case class Currency(code: String) extends AssetClass {
  def pointScale: Int = java.util.Currency.getInstance(code) match {
    case c: java.util.Currency ⇒ c.getDefaultFractionDigits
    case null ⇒ 2
  }

  override def toString = code
}
