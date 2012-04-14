package com.miriamlaurel.fxcore

import scala.collection.mutable
import java.io.Serializable

/**
 * @author Alexander Temerev
 */
trait Instrument extends Serializable {
  def primary: Asset
  def secondary: Asset

  def reverse = Instrument(secondary, primary)
}

object Instrument {

  protected val cache = mutable.Map[(Asset, Asset), Instrument]()

  def apply(primary: Asset, secondary: Asset): Instrument = cache.get((primary, secondary)) match {
    case Some(instrument) => instrument
    case None => {
      val instrument = (primary, secondary) match {
        case (a: CurrencyAsset, b: CurrencyAsset) => new CurrencyPair(a, b)
        case _ => new BaseInstrument(primary, secondary)
      }
      cache((primary, secondary)) = instrument
      instrument
    }
  }
}

case class BaseInstrument(primary: Asset, secondary: Asset) extends Instrument {
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
    CurrencyPair(CurrencyAsset(tokens(0)), CurrencyAsset(tokens(1)))
  }
}