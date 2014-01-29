package com.miriamlaurel.fxcore

import java.util.UUID

trait Entity {
  val id: UUID = UUID.randomUUID()
}

trait TimeEvent {
  val timestamp: Long = System.currentTimeMillis()
}