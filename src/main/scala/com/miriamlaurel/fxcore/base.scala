package com.miriamlaurel.fxcore

object Identity {

  private var idGen: Long = System.currentTimeMillis()

  def nextId: Long = {
    idGen = idGen + 1
    -idGen
  }
}

trait Identity {
  val id: Long
}

trait TimeEvent {
  val timestamp: Long = System.currentTimeMillis()
}