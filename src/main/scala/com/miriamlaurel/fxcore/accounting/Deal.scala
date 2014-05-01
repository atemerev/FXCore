package com.miriamlaurel.fxcore.accounting

import org.joda.time.DateTime
import com.miriamlaurel.fxcore.{Timestamp, Money}
import com.miriamlaurel.fxcore.portfolio.Position

case class Deal(position: Position, closePrice: BigDecimal, override val timestamp: DateTime, profitLoss: Money) extends Timestamp