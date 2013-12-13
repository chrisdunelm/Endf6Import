package endf6.files

import endf6.mts.EndfMt
import endf6.mts.Mt4_All

class File4(override protected val mts: Seq[EndfMt]) extends EndfFile {

  /** Elastic scattering cross-section. Section 4.1 */
  lazy val mt2opt: Option[Mt4_All] = getMts[Mt4_All](2)
  def mt2: Mt4_All = mt2opt.get

}
