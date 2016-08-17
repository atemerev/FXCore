package com.miriamlaurel.fxcore.asset

case class Currency(code: String) extends AssetClass {

  override def toString = code
}
