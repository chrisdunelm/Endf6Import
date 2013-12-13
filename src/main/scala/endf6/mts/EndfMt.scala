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
    case (3, _) => new Mt3_All(lines)
    case (4, _) => new Mt4_All(lines)
    case (6, _) => new Mt6_All(lines)
    case _ => new Unknown(lines.head.mt)
  }
  
}

trait EndfMt extends NumberedTypeName {

  def number: Int = autoNumber
  
}