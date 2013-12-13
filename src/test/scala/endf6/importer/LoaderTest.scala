package endf6.importer

import org.scalatest.FlatSpec
import scala.io.Source
import endf6.mts.Mt2_151.Raw.ScatteringRadius
import endf6.mts.EndfMt
import endf6.mts.Mt1_451
import endf6.importer.Record.Tab1.NbtInt
import endf6.importer.Record.Tab1.Xy
import endf6.mts.Mt4_All
import endf6.mts.Mt6_All.Raw.TwoBodyReactionAngularDistribution

class LoaderTest extends FlatSpec {

  "Loader" should "load neutron-1-H-1" in {
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
    assert(f3r1.nbtInts === Seq(NbtInt(30, 5), NbtInt(96, 2)))
    assert(f3r1.xys.length === 96)
    assert(f3r1.xys.take(4) ===
      Seq(Xy(1e-5, 3.713628e+1), Xy(2e-5, 3.224498e+1), Xy(5e-5, 2.790478e+1), Xy(1e-4, 2.571732e+1)))
    assert(f3r1.xys.takeRight(4) ===
      Seq(Xy(1.85e7, 5.227637e-1), Xy(1.9e7, 5.088059e-1), Xy(1.95e7, 4.954905e-1), Xy(2e+7, 4.827735e-1)))
      
    val f3r2 = mat.file3.mt2.raw
    assert(f3r2.qm === 0.0)
    assert(f3r2.qi === 0.0)
    assert(f3r2.nbtInts === Seq(NbtInt(96, 2)))
    assert(f3r2.xys.length === 96)
    assert(f3r2.xys.take(4) ===
      Seq(Xy(1e-5, 2.043634e+1), Xy(2e-5, 2.043634e+1), Xy(5e-5, 2.043634e+1), Xy(1e-4, 2.043633e+1)))
    assert(f3r2.xys.takeRight(4) ===
      Seq(Xy(1.85e7, 5.227359e-1), Xy(1.9e7, 5.087783e-1), Xy(1.95e7, 4.954630e-1), Xy(2e+7, 4.827462e-1)))
    
    val f3r102 = mat.file3.mt102.raw
    assert(f3r102.qm === 2.224631e6)
    assert(f3r102.qi === 2.224631e6)
    assert(f3r102.nbtInts === Seq(NbtInt(30, 5), NbtInt(96, 2)))
    assert(f3r102.xys.length === 96)
    assert(f3r102.xys.take(4) ===
      Seq(Xy(1e-5, 1.669994e+1), Xy(2e-5, 1.180864e+1), Xy(5e-5, 7.468441e+0), Xy(1e-4, 5.280985e+0)))
    assert(f3r102.xys.takeRight(4) ===
      Seq(Xy(1.85e7, 2.787851e-5), Xy(1.9e7, 2.766095e-5), Xy(1.95e7, 2.744259e-5), Xy(2e+7, 2.722354e-5)))
      
    val f4r2 = mat.file4.mt2.raw
    assert(f4r2.ltt === 1)
    assert(f4r2.li === 0)
    assert(f4r2.lct === 2)
    val f4r2lpc = f4r2.angularDistribution.asInstanceOf[Mt4_All.Raw.LegendrePolynomialCoefficients]
    assert(f4r2lpc.energies.length === 96)
    assert(f4r2lpc.energies(0) === Mt4_All.Raw.LegendrePolynomialCoefficients.Energy(0.0, 1.0e-5, 0,
        Seq(-1.60266e-14, 5.54677e-17, 2.13338e-17, 2.27560e-17, -7.75773e-18, -2.23184e-17)))
    assert(f4r2lpc.energies(1) === Mt4_All.Raw.LegendrePolynomialCoefficients.Energy(0.0, 2.0e-5, 0,
        Seq(-3.19039e-14, 4.35208e-17, 9.75257e-18, 2.08597e-17, -1.39639e-17, -4.06982e-17)))
    assert(f4r2lpc.energies(95) === Mt4_All.Raw.LegendrePolynomialCoefficients.Energy(0.0, 2.0e+7, 0,
        Seq(-1.608557e-2, 7.512273e-3, -1.468189e-3, 9.169295e-4, -5.764461e-5, 8.764883e-6)))
        
    val f6r102 = mat.file6.mt102.raw
    assert(f6r102.lct === 2)
    assert(f6r102.nk === 2)
    val f6r102rp = f6r102.reactionProducts
    assert(f6r102rp.length === 2)
    assert(f6r102rp(0).zap.za === 0)
    assert(f6r102rp(0).awp === 2.223300e+6)
    assert(f6r102rp(0).lip === 0)
    assert(f6r102rp(0).law === 2)
    val law2 = f6r102rp(0).distributionFunction.asInstanceOf[TwoBodyReactionAngularDistribution]
  }
  
}