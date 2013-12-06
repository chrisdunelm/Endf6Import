package endf6.mts

import endf6.importer.Record
import endf6.utils.Za

object Mt3_1 {

  class Raw(lines: Seq[Record]) {
    private val head = lines(0).asHead
    private val tab1 = lines(1).asTab1
  
    def za: Za = head.za
    def awr: Double = head.awr
    
    def qm: Double = tab1.c1
    def qi: Double = tab1.c2
    def lr: Int = tab1.l2
    
    val (_, rTable, pTable) = tab1.read(lines.drop(2))
    
  }
  
}

class Mt3_1(lines: Seq[Record]) extends EndfMt {
  import Mt3_1._
  
  val raw: Raw = new Raw(lines)

}
