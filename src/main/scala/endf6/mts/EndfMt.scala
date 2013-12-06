package endf6.mts

import endf6.importer.Record
import endf6.utils.NumberedTypeName

object EndfMt {
  import scala.reflect.runtime.universe.TypeTag
  import scala.reflect.runtime.universe.typeOf
  
  class Unknown(override val number : Int) extends EndfMt

  def create(lines: Seq[Record]): EndfMt = (lines.head.mf, lines.head.mt) match {
    case (1, 451) => new Mt1_451(lines)
    case (2, 151) => new Mt2_151(lines)
    case (3, 1) => new Mt3_1(lines)
    case _ => new Unknown(lines.head.mt)
  }
  
}

trait EndfMt extends NumberedTypeName {

  def number: Int = autoNumber
  
}