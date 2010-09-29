package com.miriamlaurel.fxcore

import java.util.Date

/**
 * @author Alexander Temerev
 */
sealed abstract class Signal(override val timestamp: Date, val refQuote: Quote) extends TimeEvent {
  def side: Int
}

case class BuySignal(ts: Date, ref: Quote) extends Signal(ts, ref) {
  def side = 1
}
case class SellSignal(ts: Date, ref: Quote) extends Signal(ts, ref) {
  def side = -1
}
case class CloseSignal(ts: Date, ref: Quote) extends Signal(ts, ref) {
  def side = 0
}
