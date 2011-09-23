package com.miriamlaurel.fxcore

import java.util.{Date, UUID}
import com.miriamlaurel.fxcore.numbers.Decimal
import java.io.Serializable

/**
 * @author Alexander Temerev
 */
case  class Offer(id: String,
            source: String,
            instrument: Instrument,
            side: OfferSide.Value,
            amount: Decimal,
            price: Decimal,
            override val timestamp: Date) extends Ordered[Offer] with Serializable with TimeEvent {

  def this(source: String,
           instrument: Instrument,
           side: OfferSide.Value,
           amount: Decimal,
           price: Decimal,
           timestamp: Date) = this (UUID.randomUUID toString, source, instrument, side, amount, price, timestamp)

  override def compare(that: Offer) = price compare (that price)
}

sealed abstract class OfferSide

object OfferSide extends Enumeration {
  val Bid, Ask = Value

  def reverse(side: OfferSide.Value): OfferSide.Value = side match {
    case Bid => Ask
    case Ask => Bid
  }
}


