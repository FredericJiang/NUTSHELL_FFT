package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._



trait HasRegVecParameter {
  val NRVec = 32
  //val VLEN = 128
} 
class RegVec extends HasRegVecParameter with HasNutCoreParameter {
  val rvec = Mem(NRVec, UInt(VLEN.W))
  def read(addr: UInt) : UInt =  rvec(addr)
  def write(addr: UInt, data: UInt, mask:UInt) = {
  rvec(addr) := (data & mask) | (rvec(addr) & ~mask)
  printf("RVEC num is %b, data is %x, write data is%x, mask is %x \n",addr,rvec(addr),data,mask)
}
}  

/* class ScoreBoard extends HasRegVecParameter {
  val busy = RegInit(0.U(NRVec.W))
  def isBusy(idx: UInt): Bool = busy(idx)
  def mask(idx: UInt) = (1.U(NRVec.W) << idx)(NRVec-1, 0)
  def update(setMask: UInt, clearMask: UInt) = {
    // When clearMask(i) and setMask(i) are both set, setMask(i) wins.
    // This can correctly record the busy bit when reg(i) is written
    // and issued at the same cycle.
    // Note that rf(0) is always free.
    busy := Cat(((busy & ~clearMask) | setMask)(NRVec-1, 1), 0.U(1.W))
  }
}
 */