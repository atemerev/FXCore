package ai.reactivity.fxcore.asset

trait AssetClass {
  def code: String

  override def toString = code
}
