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

  override def hashCode(): Int = sourceId match {
    case Some(x) => x.hashCode
    case _ => super.hashCode()
  }

  override def equals(obj: scala.Any): Boolean = sourceId match {
    case Some(x) => obj match {
      case o: Order => o.sourceId.isDefined && o.sourceId.get.equals(x)
    }
    case _ => super.equals(obj)
  }

  override def compare(that: Order) = if (side == QuoteSide.Ask) price compare that.price else that.price compare price
  override def toString = "%s %f %s @%f".format(side.toString, amount.bigDecimal, instrument.toString, price.bigDecimal)
}
