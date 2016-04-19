package com.miriamlaurel.fxcore.market

import com.miriamlaurel.fxcore.instrument.Instrument
import com.miriamlaurel.fxcore.party.Party

case class Order(key: OrderKey,
                 amount: BigDecimal,
                 price: BigDecimal) extends Ordered[Order] {

  require(amount > 0)
  require(price > 0)

  override def compare(that: Order) = if (key.side == QuoteSide.Ask) price compare that.price else that.price compare price

  override def toString = "%s %f %s @%f".format(key.side.toString, amount.bigDecimal, key.instrument.toString, price.bigDecimal)
}

object Order {
  def apply(party: Party, instrument: Instrument, side: QuoteSide.Value, id: String,
            amount: BigDecimal, price: BigDecimal): Order = Order(OrderKey(party, instrument, side, id), amount, price)
}