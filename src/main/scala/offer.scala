package com.miriamlaurel.fxcore

/**
 * @author Alexander Temerev
 */
case class Offer(instrument: Instrument,
            side: QuoteSide.Value,
            amount: BigDecimal,
            price: BigDecimal,
            source: Party = Me,
            sourceId: Option[String] = None,
            override val timestamp: Long = System.currentTimeMillis()) extends Ordered[Offer] with TimeEvent {

  require(amount > 0)
  require(price > 0)

  override def compare(that: Offer) = price compare that.price

  override def toString = "%s %f %s @%f".format(side.toString, amount.bigDecimal, instrument.toString, price.bigDecimal)
}