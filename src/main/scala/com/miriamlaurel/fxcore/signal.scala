package com.miriamlaurel.fxcore

import java.util.Date

/**
 * @author Alexander Temerev
 */
sealed abstract class Signal(override val timestamp: Date, val refQuote: Quote) extends TimeEvent {
  def side: Int
  def reverse: Signal
}

case class BuySignal(ts: Date, ref: Quote) extends Signal(ts, ref) {
  override def side = 1
  override def reverse = SellSignal(ts, ref)
  override def toString = "Buy signal @ " + ref + ", " + ts
}
case class SellSignal(ts: Date, ref: Quote) extends Signal(ts, ref) {
  override def side = -1
  override def reverse = BuySignal(ts, ref)
  override def toString = "Sell signal @ " + ref + ", " + ts
}
case class CloseSignal(ts: Date, ref: Quote) extends Signal(ts, ref) {
  override def side = 0
  override def reverse = throw new NoSuchElementException("Can't reverse a close signal")
  override def toString = "Close signal @ " + ref + ", " + ts
}
