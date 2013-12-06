package endf6.utils

import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe.typeOf
import scala.reflect.runtime.universe.Type

object NumberedTypeName {
  def getNumber(typeName: String) = typeName.reverse.takeWhile(_.isDigit).reverse.toInt
  def getNumber(tpe: Type): Int = getNumber(tpe.toString)
  def getNumber[T <: NumberedTypeName : TypeTag]: Int = getNumber(typeOf[T])
}

trait NumberedTypeName {
  import NumberedTypeName._

  def autoNumber: Int = getNumber(this.getClass.getName)
  
}