package com.miriamlaurel.fxcore.asset

object Metal {
  def apply(code: String): Metal = code match {
    case "XAU" ⇒ Gold
    case "XAG" ⇒ Silver
    case "XPT" ⇒ Platinum
    case "XPD" ⇒ Palladium
    case _ ⇒ throw new IllegalArgumentException("Metal code is not recognized: " + code)
  }
}

trait Metal extends AssetClass {
  def code: String
  override def toString = code
}

case object Gold extends Metal {
  override val code = "XAU"
}

case object Silver extends Metal {
  override val code = "XAG"
}

case object Platinum extends Metal {
  override val code = "XPT"
}

case object Palladium extends Metal {
  override val code = "XPD"
}

