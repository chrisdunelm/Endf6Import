package endf6.mts

import endf6.importer.Record
import endf6.utils.Za

object Mt6_All {

  object Raw {
    sealed trait DistributionFunction
    class UnknownDistribution extends DistributionFunction
    class ContinuumEnergyAngleDistribution extends DistributionFunction
    
    object TwoBodyReactionAngularDistribution {
      trait LegendreRepresentation {
        protected val entry: Record.List.Full
        def energy: Double = entry.list.c2
        def lang: Int = entry.list.l1
        def nl: Int = entry.list.n2
      }
      class LegendreExpansion(protected val entry: Record.List.Full) extends LegendreRepresentation {
        val coefficients: Seq[Double] = entry.result
      }
      object TabulationPMu {
        case class Entry(mu: Double, p: Double)
      }
      class TabulationPMu(protected val entry: Record.List.Full) extends LegendreRepresentation {
        import TabulationPMu._
        val entries: Seq[Entry] = (entry.result.grouped(2).map { case Seq(u, p) => Entry(u, p) }).toSeq
      }
      object TabulationLogP {
        case class Entry(mu: Double, logP: Double)
      }
      class TabulationLogP(protected val entry: Record.List.Full) extends LegendreRepresentation {
        import TabulationLogP._
        val entries: Seq[Entry] = (entry.result.grouped(2).map { case Seq(u, logP) => Entry(u, logP) }).toSeq
      }
    }
    /** Section 6.2.3 */
    class TwoBodyReactionAngularDistribution(tab2: Record.Tab2, listResult: Record.Tab2.ListResult)
        extends DistributionFunction {
      import TwoBodyReactionAngularDistribution._
      /** Don't know what these are for */
      val nbtInts: Seq[Record.Tab2.NbtInt] = listResult.nbtInts
      val energies: Seq[LegendreRepresentation] = for (entry <- listResult.data) yield {
        val lang = entry.list.l1
        lang match {
          case 0 => new LegendreExpansion(entry)
          case 12 => new TabulationPMu(entry)
          case 14 => new TabulationLogP(entry)
        }
      }
    }
    
    class IsotropicTwoBodyDistribution extends DistributionFunction
    class RecoilOfATwoBodyReactionDistribution extends DistributionFunction
    class ChargedParticleElasticDistribution extends DistributionFunction
    class NBodyPhaseSpaceDistribution extends DistributionFunction
    class LaboratoryAngleEnergyLawDistribution extends DistributionFunction
    
    class ReactionProduct(tab1Head: Record.Tab1, data: Record.Tab1.Result, distFunc: DistributionFunction) {
      /** ZA of reaction product. Some special values. Section 6.2 */
      val zap: Za = Za(tab1Head.c1)
      /** Product mass in neutron units, or energy. Section 6.2 */
      val awp: Double = tab1Head.c2
      /** Product modifier flag, mainly for isomeric states. Section 6.2 */
      val lip: Int = tab1Head.l1
      /** Representation of distribution function. Section 6.2 */
      val law: Int = tab1Head.l2
      /** Tab1 part (don't know what this is for yet). Section 6.2 */
      def tab1: Record.Tab1.Result = data
      /** Representation of distribution function. Section 6.2 */
      def distributionFunction: DistributionFunction = distFunc
    }
  }
  
  class Raw(lines: Seq[Record]) {
    import Raw._
    private val head = lines(0).asHead

    def za: Za = head.za
    def awr: Double = head.awr
    /** Reference system for secondary energy and angle (incident always LAB)
     *  1 = LAB for both, 2 = CM for both, 3 = CM for angle and energy of light particles A <= 4, lab for A > 4 */
    def lct: Int = head.l2
    /** Number of subsections in this section */
    def nk: Int = head.n1
    
    private def readUnknownDistribution(lines: Seq[Record]): (Seq[Record], DistributionFunction) = {
      ???
    }
    
    private def readContinuumEnergyAngleDistribution(lines: Seq[Record]): (Seq[Record], DistributionFunction) = {
      ???
    }
    
    private def readTwoBodyReactionAngularDistribution(lines: Seq[Record]): (Seq[Record], DistributionFunction) = {
      val tab2 = lines.head.asTab2
      val (remainingLines, listResult) = tab2.readAsList(lines.tail)
      (remainingLines, new TwoBodyReactionAngularDistribution(tab2, listResult))
    }
    
    private def readIsotropicTwoBodyDistribution(lines: Seq[Record]): (Seq[Record], DistributionFunction) = {
      ???
    }
    
    private def readRecoilOfATwoBodyReactionDistribution(lines: Seq[Record]): (Seq[Record], DistributionFunction) = {
      (lines, new RecoilOfATwoBodyReactionDistribution)
    }
    
    private def readChargedParticleElasticDistribution(lines: Seq[Record]): (Seq[Record], DistributionFunction) = {
      ???
    }
    
    private def readNBodyPhaseSpaceDistribution(lines: Seq[Record]): (Seq[Record], DistributionFunction) = {
      ???
    }
    
    private def readLaboratoryAngleEnergyLawDistribution(lines: Seq[Record]): (Seq[Record], DistributionFunction) = {
      ???
    }
    
    private def loadSubsections(lines: Seq[Record], n: Int, result: Seq[ReactionProduct]):
        (Seq[Record], Seq[ReactionProduct]) = n match {
      case 0 => (lines, result)
      case _ =>
        val tab1Head = lines.head.asTab1
        val (remainingLines, tab1Result) = tab1Head.readTab1(lines.tail)
        val law = tab1Head.l2
        val (lawRemainingLines, distributionFunction) = law match {
          case 0 => readUnknownDistribution(remainingLines)
          case 1 => readContinuumEnergyAngleDistribution(remainingLines)
          case 2 => readTwoBodyReactionAngularDistribution(remainingLines)
          case 3 => readIsotropicTwoBodyDistribution(remainingLines)
          case 4 => readRecoilOfATwoBodyReactionDistribution(remainingLines)
          case 5 => readChargedParticleElasticDistribution(remainingLines)
          case 6 => readNBodyPhaseSpaceDistribution(remainingLines)
          case 7 => readLaboratoryAngleEnergyLawDistribution(remainingLines)
        }
        val reactionProduct = new ReactionProduct(tab1Head, tab1Result, distributionFunction)
        loadSubsections(lawRemainingLines, n - 1, result :+ reactionProduct)
    }
    val (_, reactionProducts) = loadSubsections(lines.tail, nk, Seq())
  }
  
}

class Mt6_All(lines: Seq[Record]) extends EndfMt {
  import Mt6_All._
  override val number: Int = lines.head.mt
  
  val raw: Raw = new Raw(lines)

}