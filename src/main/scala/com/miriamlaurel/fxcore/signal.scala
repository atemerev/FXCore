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
  def side = 1
  def reverse = SellSignal(ts, ref)
}
case class SellSignal(ts: Date, ref: Quote) extends Signal(ts, ref) {
  def side = -1
  def reverse = BuySignal(ts, ref)
}
case class CloseSignal(ts: Date, ref: Quote) extends Signal(ts, ref) {
  def side = 0
  def reverse = throw new NoSuchElementException("Can't reverse a close signal")
}
