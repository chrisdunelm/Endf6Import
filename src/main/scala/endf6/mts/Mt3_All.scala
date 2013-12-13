package endf6.mts

import endf6.importer.Record
import endf6.utils.Za
import endf6.importer.Record.Tab1

object Mt3_All {

  class Raw(lines: Seq[Record]) {
    private val head = lines(0).asHead
    private val tab1 = lines(1).asTab1
  
    def za: Za = head.za
    def awr: Double = head.awr
    /** Mass-difference Q value (eV). Section 3.2 */
    def qm: Double = tab1.c1
    /** Reaction Q value for the (lowest energy) state defined by the given MT value in a simple
     *  2-body reaction or breakup reaction. Section 3.2 */
    def qi: Double = tab1.c2
    /** Complex or "breakup" reaction flag, which indicates that additional particles not specified
     *  by the MT number will be emitted. Section 3.2 */
    def lr: Int = tab1.l2
    
    val (_, Tab1.Result(nbtInts, xys)) = tab1.readTab1(lines.drop(2))
  }
  
}

class Mt3_All(lines: Seq[Record]) extends EndfMt {
  import Mt3_All._
  override val number: Int = lines.head.mt
  
  val raw: Raw = new Raw(lines)

}
