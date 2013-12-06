package endf6.files

import endf6.importer.Record
import endf6.mts.EndfMt
import endf6.utils.NumberedTypeName

object EndfFile {
  
  class Unknown(override val number: Int) extends EndfFile {
    override protected val mts = Seq()
  }

def create(fileNumber: Int, mts: Seq[EndfMt]): EndfFile = fileNumber match {
    case 1 => File1(mts)
    case 2 => File2(mts)
    case 3 => File3(mts)
    case _ => new Unknown(fileNumber)
  }
  
}

trait EndfFile extends NumberedTypeName {
  import scala.reflect.runtime.universe.TypeTag

  def number: Int = autoNumber
  
  protected val mts: Seq[EndfMt]
  protected lazy val mtsByNumber = mts.map(x => (x.number, x)).toMap
  
  protected def getMts[T <: EndfMt](number: Int): Option[T] = mtsByNumber.get(number).map(_.asInstanceOf[T])
  protected def getMts[T <: EndfMt : TypeTag]: Option[T] = getMts(NumberedTypeName.getNumber[T])
  
}