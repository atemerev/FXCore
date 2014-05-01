package com.miriamlaurel.fxcore

import org.joda.time.DateTime

trait Timestamp {
  val timestamp: DateTime = DateTime.now()
}