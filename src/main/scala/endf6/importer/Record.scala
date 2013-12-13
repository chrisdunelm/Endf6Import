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

  trait Text { this: Record =>
    def body: String 
  }
  
  trait Cont { this: Record =>
    lazy val (c1, c2, l1, l2, n1, n2) = body.read11s match {
      case Seq(c1, c2, l1, l2, n1, n2) => (c1.toFDouble, c2.toFDouble, l1.toFInt, l2.toFInt, n1.toFInt, n2.toFInt)
    }
  }
  
  trait Head { this: Cont =>
    lazy val za: Za = Za(c1)
    def awr: Double = c2
    def l1: Int
    def l2: Int
    def n1: Int
    def n2: Int
  }
  
  object List {
    case class Full(list: List, result: Seq[Double])
  }
  
  trait List { this: Cont =>
    def c1: Double
    def c2: Double
    def l1: Int
    def l2: Int
    def npl: Int = n1
    def n2: Int
    
    /** Read this list. Pass records starting on the line following this List record */
    def readList(lines: Seq[Record]): (Seq[Record], Seq[Double]) = {
      val plLineCount = (npl + 5) / 6
      val values = (for (i <- 0 until plLineCount)
        yield lines(i).body.read11s.take(npl - i * 6).map(_.toFDouble)).flatten
      (lines.drop(plLineCount), values)  
    }
  }
  
  trait Tab { this: Cont =>
    def nr: Int = n1
  }
  
  object Tab1 {
    case class NbtInt(nbt: Int, int: Int)
    case class Xy(x: Double, y: Double)
    case class Result(nbtInts: Seq[NbtInt], xys: Seq[Xy])
    case class Full(tab1: Tab1, result: Result)
  }
  
  trait Tab1 { this: Cont with Tab =>
    import Tab1._
    
    def c1: Double
    def c2: Double
    def l1: Int
    def l2: Int
    def nr: Int
    def np: Int = n2
    
    /** Read this tab1. Pass records starting on the line following this Tab1 record */
    def readTab1(lines: Seq[Record]): (Seq[Record], Result) = {
      val nrLineCount = (nr + 2) / 3
      val npLineCount = (np + 2) / 3
      def readTabLine[T](ofs: Int, n: Int, lineIndex: Int)(resultFn: (String, String) => T): Iterator[T] =
        lines(ofs + lineIndex).body.read11s.grouped(2).take(n - lineIndex * 3).map { case Seq(a, b) => resultFn(a, b) }
      val rTable = (for (i <- 0 until nrLineCount)
        yield readTabLine(0, nr, i) { (a, b) => NbtInt(a.toFInt, b.toFInt) }).flatten
      val pTable = (for (i <- 0 until npLineCount)
        yield readTabLine(nrLineCount, np, i) { (a, b) => Xy(a.toFDouble, b.toFDouble) }).flatten
      (lines.drop(nrLineCount + npLineCount), Result(rTable, pTable))
    }
  }
  
  object Tab2 {
    case class NbtInt(nbt: Int, int: Int)
    case class ListResult(nbtInts: Seq[NbtInt], data: Seq[List.Full])
    case class Tab1Result(nbtInts: Seq[NbtInt], data: Seq[Tab1.Full])
  }
  
  trait Tab2 { this: Cont with Tab =>
    import Tab2._

    def c1: Double
    def c2: Double
    def l1: Int
    def l2: Int
    def nr: Int
    def nz: Int = n2

    /** Read this tab2 containing lists. Pass records starting on the line following this Tab2 record */
    def readAsList(lines: Seq[Record]): (Seq[Record], ListResult) = {
      var nrLineCount = (nr + 2) / 3
      var rTable = (for (i <- 0 until nrLineCount)
        yield lines(i).body.read11s.grouped(2).take(nr - i * 3).map { case Seq(a, b) => NbtInt(a.toFInt, b.toFInt) }
      ).flatten
      def readList(lines: Seq[Record], n: Int, result: Seq[List.Full]): (Seq[Record], Seq[List.Full]) = n match {
        case 0 => (lines, result)
        case _ =>
          val list = lines.head.asList
          val (linesRemaining, data) = list.readList(lines.tail)
          readList(linesRemaining, n - 1, result :+ List.Full(list, data))
      }
      val (linesRemaining, listResult) = readList(lines.drop(nrLineCount), nz, Seq())
      (linesRemaining, ListResult(rTable, listResult))
    }
    
    /** Read this tab2 containing tab1s. Pass records starting on the line following this Tab2 record */
    def readAsTab1(lines: Seq[Record]): (Seq[Record], Tab1Result) = {
      ???
    }
    
  }
  
  private class Base(protected val line: String) extends Record
      with Text with Cont with Head with List with Tab with Tab1 with Tab2 {
    val (body, mat, mf, mt, ns) = line.read(66, 4, 2, 3, 5) match {
      case Seq(body, mat, mf, mt, ns) => (body, mat.toFInt, mf.toFInt, mt.toFInt, ns.toFInt)
    }
    override def toString = line
  }
  
  def apply(line: String): Record = new Base(line)
    
}

import Record._
trait Record { this: Text with Cont with Head with List with Tab1 with Tab2 =>
  protected val body: String
  val mat: Int
  val mf: Int
  val mt: Int
  val ns: Int
  
  def asText: Text = this
  def asCont: Cont = this
  def asHead: Head = this
  def asList: List = this
  def asTab1: Tab1 = this
  def asTab2: Tab2 = this
}
