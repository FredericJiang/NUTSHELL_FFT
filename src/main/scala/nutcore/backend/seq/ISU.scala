/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
* 
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2. 
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2 
* 
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER 
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR 
* FIT FOR A PARTICULAR PURPOSE.  
*
* See the Mulan PSL v2 for more details.  
***************************************************************************************/

package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

// Sequential Inst Issue Unit 
class ISU(implicit val p: NutCoreConfig) extends NutCoreModule with HasRegFileParameter {
  val io = IO(new Bundle {
    val in = Vec(2, Flipped(Decoupled(new DecodeIO))) // make in-order backend compatible with high performance frontend 
    val out = Decoupled(new DecodeIO)
    val wb = Flipped(new WriteBackIO)
    val forward = Flipped(new ForwardIO)
    val flush = Input(Bool())
  })

  io.out.bits := DontCare
  val rfSrc1 = io.in(0).bits.ctrl.rfSrc1
  val rfSrc2 = io.in(0).bits.ctrl.rfSrc2
  val rfDest1 = io.in(0).bits.ctrl.rfDest
  val DestType_r  = io.in(0).bits.ctrl.DestType
  val DestTypeEX_w  = io.forward.wb.DestType
  val DestTypeWB_w  = io.wb.DestType
  //增加了是否相关的条件，即是否是对同一个寄存器组进行读写
  def isDepend(rfSrc: UInt, rfDest: UInt, wen: Bool, DestType_read:UInt, DestType_write:UInt): Bool = (rfSrc =/= 0.U || DestType_read === 1.U) && (rfSrc === rfDest) && wen && (DestType_read === DestType_write)

  val forwardRfWen = io.forward.wb.rfWen && io.forward.valid //从执行级传入
  val dontForward1 = (io.forward.fuType =/= FuType.alu) && (io.forward.fuType =/= FuType.lsu) && (io.forward.fuType =/= FuType.fftu)
  val src1DependEX = isDepend(rfSrc1, io.forward.wb.rfDest, forwardRfWen, DestType_r, DestTypeEX_w)
  val src2DependEX = isDepend(rfSrc2, io.forward.wb.rfDest, forwardRfWen, DestType_r, DestTypeEX_w)
  val src1DependWB = isDepend(rfSrc1, io.wb.rfDest, io.wb.rfWen, DestType_r, DestTypeWB_w)
  val src2DependWB = isDepend(rfSrc2, io.wb.rfDest, io.wb.rfWen, DestType_r, DestTypeWB_w)

  val src1ForwardNextCycle = src1DependEX && !dontForward1
  val src2ForwardNextCycle = src2DependEX && !dontForward1
  val src1Forward = src1DependWB && Mux(dontForward1, !src1DependEX, true.B)
  val src2Forward = src2DependWB && Mux(dontForward1, !src2DependEX, true.B)

  val sb = new ScoreBoard
  val src1Ready = !sb.isBusy(rfSrc1) || src1ForwardNextCycle || src1Forward
  val src2Ready = !sb.isBusy(rfSrc2) || src2ForwardNextCycle || src2Forward
  io.out.valid := io.in(0).valid && src1Ready && src2Ready
  
  val isSRC1_counter_add  = io.in(0).bits.ctrl.fuType===FuType.fftu && io.in(0).bits.ctrl.fuOpType===FFTUOpType.vech2reg
  val isSRC1_counter_read  = io.in(0).bits.ctrl.fuType===FuType.fftu && (io.in(0).bits.ctrl.fuOpType===FFTUOpType.vech2reg || io.in(0).bits.ctrl.fuOpType===FFTUOpType.vecl2reg )
  val isclear_counter = io.in(0).bits.ctrl.fuType===FuType.fftu &&  io.in(0).bits.ctrl.fuOpType===FFTUOpType.clear_src1
  val src1_counter = RegInit(0.U(5.W))
  val pc_reg  = RegInit(0.U)
  pc_reg := io.in(0).bits.cf.pc
  val ispc_stuck = pc_reg === io.in(0).bits.cf.pc
  
  when(isSRC1_counter_add && !ispc_stuck){
    src1_counter := src1_counter + 1.U
  }
  when(isclear_counter){
    src1_counter := 0.U
  }

  val rf   = new RegFile //使用寄存器进行数据存储
  val rvec = new RegVec
//(data & mask) | (rvec(addr) & ~mask)
  val forwardEX_rvData = (io.forward.wb.rfData & io.forward.wb.mask) | (rvec.read(io.forward.wb.rfDest) & ~io.forward.wb.mask)
  val forwardWB_rvData = (io.wb.rfData & io.wb.mask) | (rvec.read(io.wb.rfDest) & ~io.wb.mask)
  

  val isSDVEC0 = (io.in(0).bits.ctrl.fuType===FuType.lsu  && io.in(0).bits.ctrl.fuOpType === LSUOpType.sdvec0)
  val isSDVEC1 = (io.in(0).bits.ctrl.fuType===FuType.lsu  && io.in(0).bits.ctrl.fuOpType === LSUOpType.sdvec1)
  val forwardEX_SDrvData = Mux(isSDVEC0,Cat(Fill(64,0.U),forwardEX_rvData(63,0)),Mux(isSDVEC1,Cat(Fill(64,0.U),forwardEX_rvData(127,64)),forwardEX_rvData))
  val forwardWB_SDrvData = Mux(isSDVEC0,Cat(Fill(64,0.U),forwardWB_rvData(63,0)),Mux(isSDVEC1,Cat(Fill(64,0.U),forwardWB_rvData(127,64)),forwardWB_rvData))
  val forwardEXData = Mux((io.in(0).bits.ctrl.DestType===0.U),io.forward.wb.rfData,forwardEX_SDrvData)
  val forwardWBData = Mux((io.in(0).bits.ctrl.DestType===0.U),io.wb.rfData,forwardWB_SDrvData) 
  // out1
  io.out.bits.data.src1 := MuxCase(1.U,Array(        
    //在这里使得操作数从rvec中得到，同时考虑数据前推的问题 
    (isSRC1_counter_read )   -> rvec.read(src1_counter),
    (io.in(0).bits.ctrl.src1Type === SrcType.pc) -> SignExt(io.in(0).bits.cf.pc, AddrBits),
    src1ForwardNextCycle -> forwardEXData, //io.forward.wb.rfData,
    (src1Forward && !src1ForwardNextCycle) -> forwardWBData, //io.wb.rfData,
    ((io.in(0).bits.ctrl.src1Type === SrcType.reg ) && !src1ForwardNextCycle && !src1Forward ) -> rf.read(rfSrc1),
    (io.in(0).bits.ctrl.src1Type === SrcType.rvec ) -> rvec.read(rfSrc1)
  ))
  io.out.bits.data.src2 :=  MuxCase(1.U,Array(
    (io.in(0).bits.ctrl.src2Type === SrcType.imm) -> io.in(0).bits.data.imm,
    src2ForwardNextCycle -> forwardEXData, //io.forward.wb.rfData,
    (src2Forward && !src2ForwardNextCycle) -> forwardWBData, //io.wb.rfData,
     (io.in(0).bits.ctrl.src2Type === SrcType.rvec) -> rvec.read(rfSrc2),
     //还没考虑数据前推的问题
     isSDVEC0 -> Cat(Fill(64,0.U),rvec.read(rfSrc2)(63,0)),
     isSDVEC1 -> Cat(Fill(64,0.U),rvec.read(rfSrc2)(127,64)),
     ((io.in(0).bits.ctrl.src2Type === SrcType.reg) && !src2ForwardNextCycle && !src2Forward) -> rf.read(rfSrc2)
    
  ))

  //when(io.in(0).bits.ctrl.src2Type === SrcType.rvec){printf("rvec %b ,data is %x \n",rfSrc2,rvec.read(rfSrc2))}

  io.out.bits.data.imm  := io.in(0).bits.data.imm

  io.out.bits.cf <> io.in(0).bits.cf
  io.out.bits.ctrl := io.in(0).bits.ctrl
  io.out.bits.ctrl.isSrc1Forward := src1ForwardNextCycle
  io.out.bits.ctrl.isSrc2Forward := src2ForwardNextCycle

  // retire: write rf
  when (io.wb.rfWen && (io.wb.DestType===0.U)) { rf.write(io.wb.rfDest, io.wb.rfData(XLEN-1,0)) }//写普通寄存器
  when (io.wb.rfWen && (io.wb.DestType===1.U)) { rvec.write(io.wb.rfDest, io.wb.rfData, io.wb.mask)}//写向量寄存器
  
  val wbClearMask = Mux(io.wb.rfWen && !isDepend(io.wb.rfDest, io.forward.wb.rfDest, forwardRfWen, DestType_r, DestTypeWB_w), sb.mask(io.wb.rfDest), 0.U(NRReg.W))
  // val isuFireSetMask = Mux(io.out.fire(), sb.mask(rfDest), 0.U)
  val isuFireSetMask = Mux(io.out.fire(), sb.mask(rfDest1), 0.U)
  when (io.flush) { sb.update(0.U, Fill(NRReg, 1.U(1.W))) }
  .otherwise { sb.update(isuFireSetMask, wbClearMask) }

  io.in(0).ready := !io.in(0).valid || io.out.fire()
  io.in(1).ready := false.B

  Debug(io.out.fire(), "issue: pc %x npc %x instr %x src1 %x src2 %x imm %x\n", io.out.bits.cf.pc, io.out.bits.cf.pnpc, io.out.bits.cf.instr, io.out.bits.data.src1, io.out.bits.data.src2, io.out.bits.data.imm)

  // read after write
  BoringUtils.addSource(io.in(0).valid && !io.out.valid, "perfCntCondMrawStall")
  BoringUtils.addSource(io.out.valid && !io.out.fire(), "perfCntCondMexuBusy")
  BoringUtils.addSource(io.out.fire(), "perfCntCondISUIssue")

  if (!p.FPGAPlatform) {
    BoringUtils.addSource(VecInit((0 to NRReg-1).map(i => rf.read(i.U))), "difftestRegs")
  }
}
