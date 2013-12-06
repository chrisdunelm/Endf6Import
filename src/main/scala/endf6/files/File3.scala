package endf6.files

import endf6.mts.EndfMt
import endf6.mts.Mt3_1

object File3 {
  def apply(mts: Seq[EndfMt]): File3 = new File3(mts)
}

class File3 private(override protected val mts: Seq[EndfMt]) extends EndfFile {

  lazy val mt1opt: Option[Mt3_1] = getMts[Mt3_1]
  def mt1: Mt3_1 = mt1opt.get

//  lazy val mt2opt: Option[Mt2] = getMts[Mt2]
//  def mt2: Mt2 = mt2opt.get
//
//  lazy val mt102opt: Option[Mt102] = getMts[Mt102]
//  def mt102: Mt102 = mt1opt.get

}
