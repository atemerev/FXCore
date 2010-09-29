package com.miriamlaurel.fxcore

import java.util.{Date, UUID}
import com.miriamlaurel.fxcore.numbers.Decimal

/**
 * @author Alexander Temerev
 */
class Offer(val id: String,
            val source: String,
            val instrument: Instrument,
            val side: OfferSide.Value,
            val amount: Decimal,
            val price: Decimal,
            override val timestamp: Date) extends Ordered[Offer] with TimeEvent {
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

object OfferSide extends Enumeration {
  val Bid, Ask = Value

  def open(value: OrderSide.Value) = if (value == OrderSide.Buy) Ask else Bid
  def close(value: OrderSide.Value) = if (value == OrderSide.Buy) Bid else Ask
}

object OrderSide extends Enumeration {
  val Buy, Sell = Value
}

