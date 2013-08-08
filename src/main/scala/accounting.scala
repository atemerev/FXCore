package com.miriamlaurel.fxcore

trait AccountingEntry extends TimeEvent {
  def change: BigDecimal
}

case class MoneyTransfer(override val timestamp: Long, change: BigDecimal) extends AccountingEntry

