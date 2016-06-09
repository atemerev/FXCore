package com.miriamlaurel.fxcore.party

case class Party(id: String, description: Option[String] = None) {
  override def hashCode(): Int = id.hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case Party(oid, _) ⇒ id.equals(oid)
    case _ ⇒ false
  }

  override def toString = description match {
    case Some(desc) ⇒ id + " " + desc
    case None ⇒ id
  }
}