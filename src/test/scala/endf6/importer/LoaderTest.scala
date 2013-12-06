package endf6.importer

import org.scalatest.FlatSpec
import scala.io.Source
import endf6.mts.Mt2_151.Raw.ScatteringRadius
import endf6.mts.EndfMt
import endf6.mts.Mt1_451
import endf6.importer.Record.Tab1.NbtInt
import endf6.importer.Record.Tab1.Xy

class LoaderTest extends FlatSpec {

  "Loader" should "load file1" in {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("n-001_H_001.endf")).getLines.toSeq
    val mat = Loader.loadMaterial(lines)
    assert(mat.z === 1)
    assert(mat.a === 1)
    assert(mat.name === "1-H-1")
    assert(mat.description.head === "  1-H -  1 LANL       EVAL-OCT05 G.M.Hale                         ")
    assert(mat.description.last === " **************************************************************** ")
    
    val f1r451 = mat.file1.mt451.raw
    assert(f1r451.lrp === 0)
    
    val f2r151 = mat.file2.mt151.raw
    assert(f2r151.nis === 1)
    assert(f2r151.isotopes.length === 1)
    assert(f2r151.isotopes(0).abn === 1.0)
    assert(f2r151.isotopes(0).lfw === 0)
    assert(f2r151.isotopes(0).ner === 1)
    assert(f2r151.isotopes(0).ranges.length === 1)
    assert(f2r151.isotopes(0).ranges(0).el === 1.0e-5)
    assert(f2r151.isotopes(0).ranges(0).eh === 1.0e+5)
    assert(f2r151.isotopes(0).ranges(0).lru === 0)
    assert(f2r151.isotopes(0).ranges(0).lrf === 0)
    assert(f2r151.isotopes(0).ranges(0).nro === 0)
    assert(f2r151.isotopes(0).ranges(0).naps === 0)
    val f2r151RangeDetail = f2r151.isotopes(0).ranges(0).detail.asInstanceOf[ScatteringRadius]
    assert(f2r151RangeDetail.spi === 0.5)
    assert(f2r151RangeDetail.ap === 1.276553)
    assert(f2r151RangeDetail.nls === 0)
    
    val f3r1 = mat.file3.mt1.raw
    assert(f3r1.qm === 0.0)
    assert(f3r1.qi === 0.0)
    assert(f3r1.rTable === Seq(NbtInt(30, 5), NbtInt(96, 2)))
    assert(f3r1.pTable.length === 96)
    assert(f3r1.pTable.take(4) ===
      Seq(Xy(1e-5, 3.713628e1), Xy(2e-5, 3.224498e1), Xy(5e-5, 2.790478e1), Xy(1e-4, 2.571732e1)))
    assert(f3r1.pTable.takeRight(4) ===
      Seq(Xy(1.85e7, 5.227637e-1), Xy(1.9e7, 5.088059e-1), Xy(1.95e7, 4.954905e-1), Xy(2e+7, 4.827735e-1)))
  }
  
}