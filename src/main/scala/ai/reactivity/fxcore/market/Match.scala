package ai.reactivity.fxcore.market

import ai.reactivity.fxcore.SafeDouble

case class Match(aggressiveSide: QuoteSide.Value, aggressiveOrderId: String, passiveOrderId: String, quantity: SafeDouble, price: SafeDouble)