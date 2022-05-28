package ai.reactivity.fxcore.market

import ai.reactivity.fxcore.SafeDouble
import ai.reactivity.fxcore.instrument.Instrument
import ai.reactivity.fxcore.party.Party

case class Order(key: OrderKey,
                 amount: SafeDouble,
                 price: SafeDouble) extends Ordered[Order] {

  override def compare(that: Order): Int = if (key.side == QuoteSide.Ask) price compare that.price else that.price compare price

  override def toString: String = "%s %s @%s".format(key.toString, amount.toDouble, price.toDouble)
}

object Order {
  def apply(party: Party, instrument: Instrument, side: QuoteSide.Value, id: String,
            amount: SafeDouble, price: SafeDouble): Order = Order(OrderKey(party, instrument, side, id), amount, price)
}