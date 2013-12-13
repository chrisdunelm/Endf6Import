package endf6.files

import endf6.mts.EndfMt
import endf6.mts.Mt3_All

class File3(override protected val mts: Seq[EndfMt]) extends EndfFile {

  /** Neutrons: Total scattering. Section 3.4.1
   *  Charged particles or photons: Photonuclear total cross-section. Section 3.5 */
  lazy val mt1opt: Option[Mt3_All] = getMts[Mt3_All](1)
  def mt1: Mt3_All = mt1opt.get

  /** Neutrons: Elastic cross-section. Section 3.4.2
   *  Charged particles or photons: N/A */
  lazy val mt2opt: Option[Mt3_All] = getMts[Mt3_All](2)
  def mt2: Mt3_All = mt2opt.get

  /** Neutrons: Radiative capture. Section 3.4 */
  lazy val mt102opt: Option[Mt3_All] = getMts[Mt3_All](102)
  def mt102: Mt3_All = mt102opt.get

}
