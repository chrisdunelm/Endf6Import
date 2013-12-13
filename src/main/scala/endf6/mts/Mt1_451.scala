package endf6.mts

import endf6.importer.Record
import endf6.utils.Za

object Mt1_451 {

  object Raw {
    case class File(mf: Int, mt: Int, nc: Int, mod: Int)
  }

  class Raw(lines: Seq[Record]) {
    import Record.StringReader
    import Raw._
    
    private val l0 = lines(0).asHead
    private val l1 = lines(1).asCont
    private val l2 = lines(2).asCont
    private val l3 = lines(3).asCont
    private val l4 = lines(4).asText
    private val l5 = lines(5).asText
  
    /** Usually 1000 * z + a. Or 1-99 for mixtures, compounds, alloys or molecules. Section 0.5.1 */
    def za: Za = l0.za
    /** Atomic number. Section 0.5.1 */
    def z: Int = za.z
    /** Mass number. 0 for multiple common isotopes. Section 0.5.1 */
    def a: Int = za.a
    /** Mass quantity, using neutron units. Section 0.5.1.1 */
    def awr: Double = l0.awr
    /** Whether resolved/unresolved resonance parameters are give in File2. Section 1.1 */
    def lrp: Int = l0.l1
    /** Whether this material fisions. Section 1.1 */
    def lfi: Boolean = l0.l2 != 0
    def nlib: Int = l0.n1
    def nmod: Int = l0.n2
    def elis: Double = l1.c1
    def sta: Double = l1.c2
    def lis: Int = l1.l1
    def lis0: Int = l1.l2
    def nfor: Int = l1.n2
    def awi: Double = l2.c1
    def emax: Double = l2.c2
    def lrel: Int = l2.l1
    def nsub: Int = l2.n1
    def nver: Int = l2.n2
    def temp: Double = l3.c1
    def ldrv: Int = l3.l1
    def nwd: Int = l3.n1
    def nxc: Int = l3.n2
    lazy val Seq(zsymam, alab, edate, auth, _) = l4.body.read(11, 11, 11, 11, 22)
    lazy val Seq(ref, ddate, rdate, endate, _) = l5.body.read(11, 11, 11, 11, 22)
    lazy val hsub: Seq[String] = for (i <- 6 to 8) yield lines(i).asText.body
    lazy val description: Seq[String] = for (i <- 4 until (4 + nwd)) yield lines(i).asText.body
    lazy val files: Seq[File] = for (i <- (4 + nwd) until (4 + nwd + nxc)) yield {
      val c = lines(i).asCont
      File(c.l1, c.l2, c.n1, c.n2)
    }
  }
  
}

class Mt1_451(lines: Seq[Record]) extends EndfMt {
  import Mt1_451._
  
  val raw: Raw = new Raw(lines)
  
  def z: Int = raw.z
  def a: Int = raw.a
  def name: String = raw.zsymam.filterNot(_ == ' ')
  def description: Seq[String] = raw.description
  
}
