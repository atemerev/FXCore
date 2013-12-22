package com.miriamlaurel.fxcore

import java.util.UUID
import java.io.Serializable

trait Entity {
  val uuid: UUID = UUID.randomUUID

  override def equals(that: Any): Boolean = that match {
    case e: Entity => this.uuid == e.uuid
    case _ => false
  }

  override def hashCode = uuid.hashCode()
}

trait TimeEvent {
  val timestamp: Long = System.currentTimeMillis()
}