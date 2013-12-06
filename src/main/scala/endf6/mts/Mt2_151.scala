package endf6.mts

import endf6.importer.Record
import endf6.utils.Za

object Mt2_151 {
  
  object Raw {
    sealed trait RangeDetail
    class ScatteringRadius(_spi: Double, _ap: Double, _nls: Int) extends RangeDetail {
      /** Spin, I, of the target nucleus. Section 2.2.1 */
      def spi: Double = _spi
      /** Scattering radius in units of 10e-12cm. Section 2.2.1 */
      def ap: Double = _ap
      /** Number of l-values (neutron orbital angular momentum) in this energy region. Section 2.2.1 */
      def nls: Int = _nls
    }
    class Range(_el: Double, _eh: Double, _lru: Int, _lrf: Int, _nro: Int, _naps: Int, val detail: RangeDetail) {
      /** Lower limit for energy range */
      def el: Double = _el
      /** Upper limit for energy range */
      def eh: Double = _eh
      /** What type of range: 0 = Scattering radius, 1 = Resolved resonance, 2 = Unresolved resonance */
      def lru: Int = _lru
      /** What representation is used for this energy range. Section 2.1 */
      def lrf: Int = _lrf
      /** Scattering radius is: 0 = Energy independent, 1 = Energy, radius pairs */
      def nro: Int = _nro
      /** Flag controlling use of two radii. Section 2.1 */
      def naps: Int = _naps
    }
    class Isotope(_zai: Za, _abn: Double, _lfw: Int, _ner: Int, _ranges: Seq[Range]) {
      /** Isotope ZA */
      def zai: Za = _zai
      /** Isotope abundance number fraction */
      def abn: Double = _abn
      /** Are average fission widths given? 0 = No, 1 = Yes */
      def lfw: Int = _lfw
      /** Number of resonance energy ranges for this isotope */
      def ner: Int = _ner
      /** All energy ranges for this isotope */
      def ranges: Seq[Range] = _ranges
    }
  }

  class Raw(lines: Seq[Record]) {
    import Raw._
    
    private val head = lines(0).asHead
  
    def za: Za = head.za
    def awr: Double = head.awr
    /** Number of isotopes in the material */
    def nis: Int = head.n1
    
    private def readScatteringRadius(lines: Seq[Record]): (RangeDetail, Seq[Record]) = {
      val line0 = lines(0).asCont
      val spi = line0.c1
      val ap = line0.c2
      val nls = line0.n1
      (new ScatteringRadius(spi, ap, nls), lines.tail)
    }
    
    private def readResolvedResonance(lines: Seq[Record]): (RangeDetail, Seq[Record]) = {
      ???
    }
    
    private def readUnresolvedResonance(lines: Seq[Record]): (RangeDetail, Seq[Record]) = {
      ???
    }
    
    private def readRanges(lines: Seq[Record], count: Int, ranges: Seq[Range]): (Seq[Range], Seq[Record]) = count match {
      case 0 => (ranges, lines)
      case _ =>
        val line0 = lines(0).asCont
        val el = line0.c1
        val eh = line0.c2
        val lru = line0.l1
        val lrf = line0.l2
        val nro = line0.n1
        val naps = line0.n2
        val (rangeDetail, unusedLines) = lru match {
          case 0 => readScatteringRadius(lines.tail)
          case 1 => readResolvedResonance(lines.tail)
          case 2 => readUnresolvedResonance(lines.tail)
        }
        val range = new Range(el, eh, lru, lrf, nro, naps, rangeDetail)
        readRanges(unusedLines, count - 1, ranges :+ range)
    }
    
    private def readIsotopes(lines: Seq[Record], count: Int, isotopes: Seq[Isotope]): Seq[Isotope] = count match {
      case 0 => isotopes
      case _ =>
        val line0 = lines(0).asCont
        val zai = Za(line0.c1)
        val abn = line0.c2
        val lfw = line0.l2
        val ner = line0.n1
        val (ranges, unusedLines) = readRanges(lines.tail, ner, Seq())
        val isotope = new Isotope(zai, abn, lfw, ner, ranges)
        readIsotopes(unusedLines, count - 1, isotopes :+ isotope)
    }
    
    val isotopes: Seq[Isotope] = readIsotopes(lines.tail, nis, Seq())

  }

}

class Mt2_151(lines: Seq[Record]) extends EndfMt {
  import Mt2_151._
  
  val raw: Raw = new Raw(lines)

}
