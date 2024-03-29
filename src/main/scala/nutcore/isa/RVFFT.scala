package nutcore

import chisel3._
import chisel3.util._

object RVFFTInstr extends HasInstrType with HasNutCoreParameter {
 // def custom opcode = (0001011,0101011,1011011,1111011)
  def setLength         = BitPat("b???????_?????_????_?_000_?????_0001011")
  def test_write1 = BitPat("b????_????_????_????_0_001_?????_0001011")
  def test_write2 = BitPat("b????_????_????_????_1_001_?????_0001011")
  def test_write3 = BitPat("b????_????_????_????_0_010_?????_0001011")
  def test_write4 = BitPat("b????_????_????_????_1_010_?????_0001011")
  def test_write5 = BitPat("b????_????_????_????_0_011_?????_0001011")
  def test_write6 = BitPat("b????_????_????_????_1_011_?????_0001011")
  def test_write7 = BitPat("b????_????_????_????_0_100_?????_0001011")
  def test_write8 = BitPat("b????_????_????_????_1_100_?????_0001011")
  
  def reg2vec          = BitPat("b???????_?????_?????_000_?????_0101011")
  def vecl2reg         = BitPat("b???????_?????_?????_001_?????_0101011")
  def vech2reg         = BitPat("b???????_?????_?????_010_?????_0101011")
  def clear_counter    = BitPat("b0000000_00000_00000_111_00000_0101011")
  def clear_src1       = BitPat("b1111111_11111_11111_111_00000_0101011")

  def complex_mul0          = BitPat("b???????_?????_?????_100_?????_0101011")
  def complex_mul1          = BitPat("b???????_?????_?????_101_?????_0101011")
  def complex_mul2          = BitPat("b???????_?????_?????_110_?????_0101011")
  
  //def complex_mul          = BitPat("b???????_?????_?????_111_?????_0001011")
  def shuffle_single       = BitPat("b????000_?????_?????_110_?????_0001011")
  def shuffle_double0      = BitPat("b????001_?????_?????_110_?????_0001011")
  def shuffle_double1      = BitPat("b????010_?????_?????_110_?????_0001011")
  def butterfly_single0    = BitPat("b????000_?????_?????_101_?????_0001011")
  def butterfly_single1    = BitPat("b????001_?????_?????_101_?????_0001011")
  def butterfly_double    = BitPat("b????011_?????_?????_101_?????_0001011")
  def write_buffer   = BitPat("b????111_?????_?????_101_?????_0001011")//配合buffterfly0 shuffle0使用，将另一半128bits存入rvec中
  
       

  val table = Array(
    setLength          -> List(InstrFFT, FuType.fftu, FFTUOpType.setLength),
    test_write1        -> List(InstrFFT, FuType.fftu, FFTUOpType.test_write1),
    test_write2        -> List(InstrFFT, FuType.fftu, FFTUOpType.test_write2),
    test_write3        -> List(InstrFFT, FuType.fftu, FFTUOpType.test_write3),
    test_write4        -> List(InstrFFT, FuType.fftu, FFTUOpType.test_write4),
    test_write5        -> List(InstrFFT, FuType.fftu, FFTUOpType.test_write5),
    test_write6        -> List(InstrFFT, FuType.fftu, FFTUOpType.test_write6),
    test_write7        -> List(InstrFFT, FuType.fftu, FFTUOpType.test_write7),
    test_write8        -> List(InstrFFT, FuType.fftu, FFTUOpType.test_write8),
    butterfly_single0  -> List(InstrFFT, FuType.fftu, FFTUOpType.butterfly_single0),
    butterfly_single1  -> List(InstrFFT, FuType.fftu, FFTUOpType.butterfly_single1),
    //butterfly3         -> List(InstrFFT, FuType.fftu, FFTUOpType.butterfly3),
    butterfly_double         -> List(InstrFFT, FuType.fftu, FFTUOpType.butterfly_double),
    write_buffer       -> List(InstrFFT, FuType.fftu, FFTUOpType.write_buffer),
    shuffle_single     -> List(InstrFFT, FuType.fftu, FFTUOpType.shuffle_single),
    shuffle_double0    -> List(InstrFFT, FuType.fftu, FFTUOpType.shuffle_double0),
    shuffle_double1           -> List(InstrFFT, FuType.fftu, FFTUOpType.shuffle_double1),
    //shuffle0           -> List(InstrFFT, FuType.fftu, FFTUOpType.shuffle0),
    complex_mul0        -> List(InstrFFT, FuType.fftu, FFTUOpType.complex_mul0),
    complex_mul1        -> List(InstrFFT, FuType.fftu, FFTUOpType.complex_mul1),
    complex_mul2        -> List(InstrFFT, FuType.fftu, FFTUOpType.complex_mul2),
    reg2vec            -> List(InstrFFT, FuType.fftu, FFTUOpType.reg2vec),
    vecl2reg           -> List(InstrFFT, FuType.fftu, FFTUOpType.vecl2reg),
    vech2reg           -> List(InstrFFT, FuType.fftu, FFTUOpType.vech2reg),
    clear_counter      -> List(InstrFFT, FuType.fftu, FFTUOpType.clear_counter),
    clear_src1         -> List(InstrFFT, FuType.fftu, FFTUOpType.clear_src1)

  )
}
