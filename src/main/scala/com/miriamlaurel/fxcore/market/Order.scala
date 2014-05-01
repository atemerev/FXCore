package com.miriamlaurel.fxcore.market

import com.miriamlaurel.fxcore.instrument.Instrument
import com.miriamlaurel.fxcore.party.Party
import com.miriamlaurel.fxcore.{Timestamp, Identity, Me}
import java.util.UUID
import org.joda.time.DateTime

case class Order(instrument: Instrument,
                       side: QuoteSide.Value,
                       amount: BigDecimal,
                       price: BigDecimal,
                       source: Party = Me,
                       sourceId: Option[String] = None,
                       override val timestamp: DateTime = DateTime.now(),
                       override val id: UUID = UUID.randomUUID()) extends Ordered[Order] with Identity with Timestamp {

  require(amount > 0)
  require(price > 0)

  override def compare(that: Order) = price compare that.price
  override def toString = "%s %f %s @%f".format(side.toString, amount.bigDecimal, instrument.toString, price.bigDecimal)
}
