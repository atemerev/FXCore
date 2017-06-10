package com.miriamlaurel.fxcore.market

import com.miriamlaurel.fxcore.SafeDouble
import com.miriamlaurel.fxcore.instrument.Instrument
import com.miriamlaurel.fxcore.party.Party

case class Order(key: OrderKey,
                 amount: SafeDouble,
                 price: SafeDouble) extends Ordered[Order] {

  require(amount > 0)
  require(price > 0)

  override def compare(that: Order): Int = if (key.side == QuoteSide.Ask) price compare that.price else that.price compare price

  override def toString: String = "%s %f @%f".format(key.toString, amount.toDouble, price.toDouble)
}

object Order {
  def apply(party: Party, instrument: Instrument, side: QuoteSide.Value, id: String,
            amount: SafeDouble, price: SafeDouble): Order = Order(OrderKey(party, instrument, side, id), amount, price)
}