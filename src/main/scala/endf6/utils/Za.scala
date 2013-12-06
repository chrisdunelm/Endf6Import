package endf6.utils

object Za {
  def apply(za: Double): Za = Za(za.toInt)
}

case class Za(za: Int) {
  val z: Int = za / 1000
  val a: Int = za % 1000
}