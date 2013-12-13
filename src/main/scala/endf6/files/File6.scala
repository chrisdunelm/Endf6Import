package endf6.files

import endf6.mts.EndfMt
import endf6.mts.Mt6_All

class File6(override protected val mts: Seq[EndfMt]) extends EndfFile {

  /** Elastic scattering cross-section. Section 4.1 */
  lazy val mt102opt: Option[Mt6_All] = getMts[Mt6_All](102)
  def mt102: Mt6_All = mt102opt.get

}
