package endf6.importer

import endf6.files.EndfFile
import endf6.Material
import endf6.mts.EndfMt

object Loader {

  def loadMaterial(s: String): Material = loadMaterial(s.split("\n"))
  
  def loadMaterial(lines: Seq[String]): Material = {
    
    def loadMts(lines: Seq[Record], mts: Seq[EndfMt]): Seq[EndfMt] = lines.dropWhile(_.mt == 0) match {
      case lines @ Seq(head, _*) =>
        val mtLines = lines.takeWhile(_.mt != 0)
        val mt = EndfMt.create(mtLines)
        loadMts(lines.drop(mtLines.length), mts :+ mt)
      case _ => mts
    }
    
    def loadFiles(lines: Seq[Record], files: Seq[EndfFile]): Seq[EndfFile] = lines.dropWhile(_.mf == 0) match {
      case lines @ Seq(head, _*) =>
        val fileLines = lines.takeWhile(_.mf != 0)
        val mts = loadMts(fileLines, Seq())
        val file = EndfFile.create(head.mf, mts)
        loadFiles(lines.drop(fileLines.length), files :+ file)
      case _ => files
    }

    val allFiles = loadFiles(lines.map(x => new Record(x)), Seq())
    new Material(allFiles)
  }

}