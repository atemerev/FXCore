package com.miriamlaurel.fxcore

import java.util.UUID
import com.miriamlaurel.fxcore.numbers.Decimal
import java.io.Serializable

/**
 * @author Alexander Temerev
 */
class Offer(val source: String,
            val instrument: Instrument,
            val side: OfferSide.Value,
            val amount: Decimal,
            val price: Decimal,
            override val uuid: UUID = UUID.randomUUID(),
            override val timestamp: Long) extends Ordered[Offer] with Entity with TimeEvent {

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