package endf6.importer

import endf6.utils.Za

object Record {
  implicit class StringReader(val s: String) extends AnyVal {
    def read(widths: Int*): Seq[String] = {
      if (s.length != widths.sum) {
        throw new Exception(s"String '$s' width = ${s.length}, widths total = ${widths.sum}")
      }
      widths.foldLeft((Seq[String](), 0)) { case ((res, tw), width) =>
        (res :+ s.substring(tw, tw + width), tw + width)
        }._1
    }
    def read11s: Seq[String] = read(11, 11, 11, 11, 11, 11)
    /** Convert string in FORTRAN style to int */
    def toFInt: Int = s.trim.toInt
    /** Convert string in FORTRAN style to double */
    def toFDouble: Double = {
      val a = s.trim
      val toConvert = math.max(a.indexOf('+', 1), a.indexOf('-', 1)) match {
        case ePos if ePos >= 0 => a.substring(0, ePos) + "e" + a.substring(ePos)
        case _ => a
      }
      toConvert.toDouble
    }
  }

  class Cont(val record: Record) {
    val (c1, c2, l1, l2, n1, n2) = record.body.read11s match {
      case Seq(c1, c2, l1, l2, n1, n2) => (c1.toFDouble, c2.toFDouble, l1.toFInt, l2.toFInt, n1.toFInt, n2.toFInt)
    }
  }
  
  class Head(val cont: Cont) {
    val za: Za = Za(cont.c1)
    def awr: Double = cont.c2
    def l1: Int = cont.l1
    def l2: Int = cont.l2
    def n1: Int = cont.n1
    def n2: Int = cont.n2
  }
  
  object Tab1 {
    case class NbtInt(nbt: Int, int: Int)
    case class Xy(x: Double, y: Double)
  }
  
  class Tab1(val record: Record) {
    import Tab1._
    val (c1, c2, l1, l2, nr, np) = record.body.read11s match {
      case Seq(c1, c2, l1, l2, nr, np) => (c1.toFDouble, c2.toFDouble, l1.toFInt, l2.toFInt, nr.toFInt, np.toFInt)
    }
    /** Read this tab1. Pass records starting on the line following this Tab1 record */
    def read(lines: Seq[Record]): (Seq[Record], Seq[NbtInt], Seq[Xy]) = {
      val nrLineCount = (nr + 2) / 3
      val npLineCount = (np + 2) / 3
      def readTabLine[T](ofs: Int, n: Int, lineIndex: Int)(resultFn: (String, String) => T): Iterator[T] =
        lines(ofs + lineIndex).body.read11s.grouped(2).zipWithIndex.takeWhile(lineIndex * 3 + _._2 < n).map {
          case (Seq(a, b), _) => resultFn(a, b)
        }
      val rTable = (for (i <- 0 until nrLineCount)
        yield readTabLine(0, nr, i) { (a, b) => NbtInt(a.toFInt, b.toFInt) }).flatten
      val pTable = (for (i <- 0 until npLineCount)
        yield readTabLine(nrLineCount, np, i) { (a, b) => Xy(a.toFDouble, b.toFDouble) }).flatten
      (lines.drop(nrLineCount + npLineCount), rTable, pTable)
    }
  }
    
}
  
class Record(val line: String) {
  import Record._
  val (body, mat, mf, mt, ns) = line.read(66, 4, 2, 3, 5) match {
    case Seq(body, mat, mf, mt, ns) => (body, mat.toFInt, mf.toFInt, mt.toFInt, ns.toFInt)
  }
  
  lazy val asCont: Cont = new Cont(this)
  lazy val asHead: Head = new Head(asCont)
  lazy val asTab1: Tab1 = new Tab1(this)
  
}
