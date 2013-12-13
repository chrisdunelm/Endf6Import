package endf6

import endf6.files.EndfFile
import endf6.files.File1
import endf6.files.File2
import endf6.files.File3
import endf6.utils.NumberedTypeName
import endf6.files.File4
import endf6.files.File6

class Material(files: Seq[EndfFile]) {
  import scala.reflect.runtime.universe.TypeTag

  private val filesByNumber = files.map(x => (x.number, x)).toMap
  
  private def getFile[T <: EndfFile](number: Int): Option[T] = filesByNumber.get(number).map(_.asInstanceOf[T])
  private def getFile[T <: EndfFile : TypeTag]: Option[T] = getFile(NumberedTypeName.getNumber[T])
  
  lazy val file1opt: Option[File1] = getFile[File1]
  def file1: File1 = file1opt.get
  lazy val file2opt: Option[File2] = getFile[File2]
  def file2: File2 = file2opt.get
  lazy val file3opt: Option[File3] = getFile[File3]
  def file3: File3 = file3opt.get
  lazy val file4opt: Option[File4] = getFile[File4]
  def file4: File4 = file4opt.get
  lazy val file6opt: Option[File6] = getFile[File6]
  def file6: File6 = file6opt.get
  
  def z: Int = file1.z
  def a: Int = file1.a
  def name: String = file1.name
  def description: Seq[String] = file1.description
  
}