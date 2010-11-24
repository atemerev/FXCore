package com.miriamlaurel.fxcore

import java.util.{Date, UUID}
import com.miriamlaurel.fxcore.numbers.Decimal
import java.io.Serializable

/**
 * @author Alexander Temerev
 */
class Offer(val id: String,
            val source: String,
            val instrument: Instrument,
            val side: OfferSide.Value,
            val amount: Decimal,
            val price: Decimal,
            override val timestamp: Date) extends Ordered[Offer] with Serializable with TimeEvent {

  def this(source: String,
           instrument: Instrument,
           side: OfferSide.Value,
           amount: Decimal,
           price: Decimal,
           timestamp: Date) = this (UUID.randomUUID toString, source, instrument, side, amount, price, timestamp)

  override def equals(obj: Any) = obj.isInstanceOf[Offer] && id == obj.asInstanceOf[Offer].id

  override def hashCode = id hashCode

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


