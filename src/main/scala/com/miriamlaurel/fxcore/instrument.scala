package com.miriamlaurel.fxcore

import scala.collection.mutable.Map

/**
 * @author Alexander Temerev
 */
abstract class Instrument {
  def primary: Asset
  def secondary: Asset

  def reverse = Instrument(secondary, primary)
}

object Instrument {

  protected val cache = Map[Tuple2[Asset, Asset], Instrument]()

  def apply(primary: Asset, secondary: Asset): Instrument =
    if (cache.contains((primary, secondary))) cache((primary, secondary)) else {
      val instrument = (primary, secondary) match {
        case (a: CurrencyAsset, b: CurrencyAsset) => new CurrencyPair(a, b)
        case _ => new BaseInstrument(primary, secondary)
      }
      cache((primary, secondary)) = instrument
      instrument
    }
}

class BaseInstrument(val primary: Asset, val secondary: Asset) extends Instrument {

  lazy val toCurrencyPair = new CurrencyPair(primary.asInstanceOf[CurrencyAsset], secondary.asInstanceOf[CurrencyAsset])
}

case class CurrencyPair(override val primary: CurrencyAsset, override val secondary: CurrencyAsset)
        extends BaseInstrument(primary, secondary) {

  override def toString = primary.toString + "/" + secondary.toString
}

object CurrencyPair {

  def apply(ticker:String):CurrencyPair = {
    require(ticker.contains("/"))
    val tokens = ticker.split("/")
    return new CurrencyPair(CurrencyAsset(tokens(0)), CurrencyAsset(tokens(1)))
  }
}

