package endf6.mts

import endf6.importer.Record
import endf6.utils.Za

object Mt4_All {
  
  object Raw {
    sealed trait AngularDistribution
    object LegendrePolynomialCoefficients {
      case class Energy(t: Double, e: Double, lt: Int, values: Seq[Double])
    }
    class LegendrePolynomialCoefficients(data: Record.Tab2.ListResult) extends AngularDistribution {
      import LegendrePolynomialCoefficients._
      val energies: Seq[Energy] = for (item <- data.data)
        yield Energy(item.list.c1, item.list.c2, item.list.l1, item.result)
    }
    class TabulatedProbabilityDistributions extends AngularDistribution
  }
  
  class Raw(lines: Seq[Record]) {
    import Raw._
    private val head = lines(0).asHead
    private val l1 = lines(1).asCont

    def za: Za = head.za
    def awr: Double = head.awr
    /** Flag to specify the representation used. Section 4.2 */
    def ltt: Int = head.l2
    /** Flag to specify whether angular distributions: 0 = not all isotropic, 1 = all isotropic */
    def li: Int = l1.l1
    /** Flag to specify frame of reference: 1 = LAB system, 2 = CM system */
    def lct: Int = l1.l2
    
    private def readLegendrePolynomialCoefficients(lines: Seq[Record]):
        (Seq[Record], LegendrePolynomialCoefficients) = {
      val tab2 = lines.head.asTab2
      val (linesRemaining, data) = tab2.readAsList(lines.tail)
      (linesRemaining, new LegendrePolynomialCoefficients(data))
    }
    
    private def readTabulatedProbabilityDistributions(lines: Seq[Record]):
        (Seq[Record], TabulatedProbabilityDistributions) = {
      ???
    }
    
    val angularDistribution: AngularDistribution = (ltt, li) match {
      case (1, 0) => readLegendrePolynomialCoefficients(lines.drop(2))._2
      case (2, 0) => readTabulatedProbabilityDistributions(lines.drop(2))._2
    }
    
  }
}

class Mt4_All(lines: Seq[Record]) extends EndfMt {
  import Mt4_All._
  override val number: Int = lines.head.mt
  
  val raw: Raw = new Raw(lines)

}
