package com.miriamlaurel.fxcore

import java.util.UUID

trait Identity {
  val id: UUID = UUID.randomUUID()
}
