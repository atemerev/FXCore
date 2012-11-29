package com.miriamlaurel.fxcore

import com.miriamlaurel.fxcore.numbers.Decimal
import java.util.UUID

/**
 * @author Alexander Temerev
 */
class Offer(val instrument: Instrument,
            val side: QuoteSide.Value,
            val amount: Decimal,
            val price: Decimal,
            val source: Party = Me,
            override val uuid: UUID = UUID.randomUUID(),
            override val timestamp: Long) extends Ordered[Offer] with Entity with TimeEvent {

  require(amount > 0)
  require(price > 0)

  override def compare(that: Offer) = price compare (that price)
}