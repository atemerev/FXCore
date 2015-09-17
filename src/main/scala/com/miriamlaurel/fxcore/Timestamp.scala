package com.miriamlaurel.fxcore

import java.time.Instant

trait Timestamp {
  val timestamp: Instant = Instant.now()
}
