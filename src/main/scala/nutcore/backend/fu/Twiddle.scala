package nutcore
import chisel3._
import chisel3.util._
import chisel3.experimental._
import scala.math._
import chisel3.util.experimental.BoringUtils


trait Twiddle{
  val FFTLength = 1024
//Wn = cosx + sin(-x) j
  def sinTable(k: Int): Vec[FixedPoint] = {
    val times = (0 until FFTLength/2 by pow(2, (10-k)).toInt)
                .map(i => -(i * 2 * Pi) / FFTLength.toDouble)
    val inits = times
                .map(t => FixedPoint.fromDouble(sin(t), 12.W, 10.BP))
    VecInit(inits)
  }

  def cosTable(k: Int): Vec[FixedPoint] = {
    val times = (0 until FFTLength/2 by pow(2, (10-k)).toInt)
                .map(i => (i * 2 * Pi) / FFTLength.toDouble)
    val inits = times
                .map(t => FixedPoint.fromDouble(cos(t), 12.W, 10.BP))
    VecInit(inits)
  }

  def wnTable(k: Int)(idx: UInt): UInt = {
    Cat(sinTable(k)(idx),cosTable(k)(idx))
  }

}

object complex_multiplier {
  def apply(src1: UInt, wn: UInt): UInt = {

  // val src1_real = Cat(Fill(4,src1(15)),src1(15,0))
  // val src1_img  = Cat(Fill(4,src1(31)),src1(31,16))

  val src1_real = src1(15,0)
  val src1_img  = src1(31,16)
  val wn_real   = Cat(Fill(2,wn(11)),wn(11,0))
  val wn_img    = Cat(Fill(2,wn(23)),wn(23,12))

  val new_real  = WireInit(0.U(30.W))
  val new_img   = WireInit(0.U(30.W))
  
  new_real := (src1_real*wn_real)- (src1_img*wn_img)
  new_img  := (src1_real*wn_img) + (src1_img*wn_real)

  Cat(new_img(27,12),new_real(27,12))

  }
}


object complex_add {
  def apply(src1: UInt, src2: UInt): UInt = {
  //采用双符号位设计
    val num1r = Cat(src1(15),src1(15,0))
    val num1i = Cat(src1(31),src1(31,16))
    val num2r = Cat(src2(15),src2(15,0))
    val num2i = Cat(src2(31),src2(31,16))
    val resultr = WireInit(0.U(17.W))
    val resulti = WireInit(0.U(17.W))
    val outr = WireInit(0.U(16.W))
    val outi = WireInit(0.U(16.W))

     resultr := num1r + num2r
     resulti := num1i + num2i
  
  //两者均为正数或负数才会溢出
    when(num1r(16,15)===num2r(16,15)){
      when(resultr(16,15)===2.U){
        outr := 0x8001.U
      }
      .elsewhen(resultr(16,15)===1.U){
        outr := 0x7fff.U
      }.otherwise{
        outr := resultr(15,0)
      }
    }.otherwise{
      outr := resultr(15,0)
    }

    when(num1i(16,15)===num2i(16,15)){
      when(resulti(16,15)===2.U){
        outi := 0x8001.U
      }
      .elsewhen(resulti(16,15)===1.U){
        outi := 0x7fff.U
      }.otherwise{
        outi := resulti(15,0)
      }
    }.otherwise{
      outi := resulti(15,0)
    }
    
        Cat(outi,outr)

  }
}

object complex_sub {
  def apply(src1: UInt, src2: UInt): UInt = {
    val num1r = src1(15,0)
    val num1i = src1(31,16)
    val num2r = src2(15,0)
    val num2i = src2(31,16)
    val resultr = WireInit(0.U(16.W))
    val resulti = WireInit(0.U(16.W))

    resultr := num1r - num2r
    resulti := num1i - num2i
    Cat(resulti,resultr)

  }
}



