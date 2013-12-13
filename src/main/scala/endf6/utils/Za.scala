package endf6.utils

object Za {
  def apply(za: Double): Za = Za(za.toInt)
}

case class Za(za: Int) {
  def z: Int = za / 1000
  def a: Int = za % 1000
}