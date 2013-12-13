package endf6.files

import endf6.mts.EndfMt
import endf6.mts.Mt2_151

class File2(override protected val mts: Seq[EndfMt]) extends EndfFile {

  lazy val mt151opt: Option[Mt2_151] = getMts[Mt2_151]
  def mt151: Mt2_151 = mt151opt.get

}
