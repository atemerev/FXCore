package com.miriamlaurel.fxcore

import java.io.Serializable

/**
 * @author Alexander Temerev
 */
trait AssetClass extends Serializable {
  def code: String
  override def toString = code
}

case class Currency(code: String) extends AssetClass {
  def pointScale: Int = java.util.Currency.getInstance(code) match {
    case c: java.util.Currency => c.getDefaultFractionDigits
    case null => 2
  }
}

trait Metal extends AssetClass

case object Gold extends Metal {
  val code = "XAU"
}

case object Silver extends Metal {
  val code = "XAG"
}

case object Platinum extends Metal {
  val code = "XPT"
}

case object Palladium extends Metal {
  val code = "XPD"
}

object Metal {
  def apply(code: String): Metal = code match {
    case "XAU" => Gold
    case "XAG" => Silver
    case "XPT" => Platinum
    case "XPD" => Palladium
    case _     => throw new IllegalArgumentException("Metal code is not recognized: " + code)
  }
}