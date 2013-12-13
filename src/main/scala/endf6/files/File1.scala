package endf6.files

import endf6.importer.Record
import endf6.mts.EndfMt
import endf6.mts.Mt1_451

class File1(override protected val mts: Seq[EndfMt]) extends EndfFile {

  lazy val mt451opt: Option[Mt1_451] = getMts[Mt1_451]
  def mt451: Mt1_451 = mt451opt.get
  
  def z: Int = mt451.z
  def a: Int = mt451.a
  def name: String = mt451.name
  def description: Seq[String] = mt451.description
  
}