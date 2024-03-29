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
import bus.simplebus._
import top.Settings

class EXU(implicit val p: NutCoreConfig) extends NutCoreModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new DecodeIO))
    val out = Decoupled(new CommitIO)
    val flush = Input(Bool())
    val dmem = new SimpleBusUC(addrBits = VAddrBits)
    val forward = new ForwardIO
    val memMMU = Flipped(new MemMMUIO)
  })
  val pc_reg = RegInit(0.U)
  pc_reg := io.in.bits.cf.pc
  val src1  = io.in.bits.data.src1(XLEN-1,0)
  val src2  = io.in.bits.data.src2(XLEN-1,0)
  val vsrc1 = io.in.bits.data.src1(VLEN-1,0)
  val vsrc2 = io.in.bits.data.src2(VLEN-1,0)

  val (fuType, fuOpType) = (io.in.bits.ctrl.fuType, io.in.bits.ctrl.fuOpType)

  val fuValids = Wire(Vec(FuType.num, Bool()))
  (0 until FuType.num).map (i => fuValids(i) := (fuType === i.U) && io.in.valid && !io.flush)

  val alu = Module(new ALU(hasBru = true))
  val aluOut = alu.access(valid = fuValids(FuType.alu), src1 = src1, src2 = src2, func = fuOpType)
  alu.io.cfIn := io.in.bits.cf
  alu.io.offset := io.in.bits.data.imm
  alu.io.out.ready := true.B
  
  val fftu =  Module(new FFTU)
  val fftuOut = fftu.access(valid = fuValids(FuType.fftu), src1 = vsrc1, src2 = vsrc2, func = fuOpType)
  fftu.io.cfIn := io.in.bits.cf
  fftu.io.parameters :=  ZeroExt(io.in.bits.data.imm,VLEN)
  fftu.io.out.ready := true.B



  def isBru(func: UInt) = func(4)

  val lsu = Module(new UnpipelinedLSU)
  val lsuTlbPF = WireInit(false.B)
  val lsuOut = lsu.access(valid = fuValids(FuType.lsu), src1 = src1, src2 = io.in.bits.data.imm, func = fuOpType, dtlbPF = lsuTlbPF)
  lsu.io.wdata := src2
  lsu.io.instr := io.in.bits.cf.instr
  io.out.bits.isMMIO := lsu.io.isMMIO || (AddressSpace.isMMIO(io.in.bits.cf.pc) && io.out.valid)
  io.dmem <> lsu.io.dmem
  lsu.io.out.ready := true.B
  


  val mdu = Module(new MDU)
  val mduOut = mdu.access(valid = fuValids(FuType.mdu), src1 = src1, src2 = src2, func = fuOpType)
  mdu.io.out.ready := true.B

  // val csr = if (Settings.get("MmodeOnly")) Module(new CSR_M) else Module(new CSR)
  val csr = Module(new CSR)
  val csrOut = csr.access(valid = fuValids(FuType.csr), src1 = src1, src2 = src2, func = fuOpType)
  csr.io.cfIn := io.in.bits.cf
  csr.io.cfIn.exceptionVec(loadAddrMisaligned)  := lsu.io.loadAddrMisaligned
  csr.io.cfIn.exceptionVec(storeAddrMisaligned) := lsu.io.storeAddrMisaligned
  csr.io.instrValid := io.in.valid && !io.flush
  csr.io.isBackendException := false.B
  io.out.bits.intrNO := csr.io.intrNO
  csr.io.isBackendException := false.B
  csr.io.out.ready := true.B

  csr.io.imemMMU <> io.memMMU.imem
  csr.io.dmemMMU <> io.memMMU.dmem

  val mou = Module(new MOU)
  // mou does not write register
  mou.access(valid = fuValids(FuType.mou), src1 = src1, src2 = src2, func = fuOpType)
  mou.io.cfIn := io.in.bits.cf
  mou.io.out.ready := true.B
  

  val isFFTreg2vec = io.in.bits.ctrl.fuType===FuType.fftu && io.in.bits.ctrl.fuOpType === FFTUOpType.reg2vec && (pc_reg =/= io.in.bits.cf.pc)

  io.out.bits.decode := DontCare
  // (io.out.bits.decode.ctrl, io.in.bits.ctrl) match { case (o, i) =>
  //   o.rfWen := i.rfWen && (!lsuTlbPF && !lsu.io.loadAddrMisaligned && !lsu.io.storeAddrMisaligned || !fuValids(FuType.lsu)) && !(csr.io.wenFix && fuValids(FuType.csr))
  //   o.rfDest := Mux(isFFTreg2vec ,fftu.io.counter_dest ,i.rfDest)
  //   o.rfDest := i.rfDest
  //   o.fuType := i.fuType
  //   o.DestType := i.DestType
  // }

  io.out.bits.decode.ctrl.rfWen := io.in.bits.ctrl.rfWen && (!lsuTlbPF && !lsu.io.loadAddrMisaligned && !lsu.io.storeAddrMisaligned || !fuValids(FuType.lsu)) && !(csr.io.wenFix && fuValids(FuType.csr))
  io.out.bits.decode.ctrl.rfDest := Mux(isFFTreg2vec ,fftu.io.counter_dest ,io.in.bits.ctrl.rfDest)
  io.out.bits.decode.ctrl.fuType := io.in.bits.ctrl.fuType
  io.out.bits.decode.ctrl.DestType := io.in.bits.ctrl.DestType

  io.out.bits.decode.cf.pc := io.in.bits.cf.pc
  io.out.bits.decode.cf.instr := io.in.bits.cf.instr
  io.out.bits.decode.cf.redirect <>
    Mux(mou.io.redirect.valid, mou.io.redirect,
      Mux(csr.io.redirect.valid, csr.io.redirect, alu.io.redirect))
  
  Debug(mou.io.redirect.valid || csr.io.redirect.valid || alu.io.redirect.valid, "[REDIRECT] mou %x csr %x alu %x \n", mou.io.redirect.valid, csr.io.redirect.valid, alu.io.redirect.valid)
  Debug(mou.io.redirect.valid || csr.io.redirect.valid || alu.io.redirect.valid, "[REDIRECT] flush: %d mou %x csr %x alu %x\n", io.flush, mou.io.redirect.target, csr.io.redirect.target, alu.io.redirect.target)

  // FIXME: should handle io.out.ready == false
  io.out.valid := io.in.valid && MuxLookup(fuType, true.B, List(
    FuType.lsu -> lsu.io.out.valid,
    FuType.mdu -> mdu.io.out.valid,
    FuType.fftu -> fftu.io.out.valid
  ))
  
  val isvecL = fuType===FuType.lsu && (fuOpType === LSUOpType.ldvec0 )
  val isvecH = fuType===FuType.lsu && (fuOpType === LSUOpType.ldvec1 )
  val lsuOut1 = Mux(isvecH, Cat(lsuOut(63,0),Fill(64,0.U)), lsuOut)
  val lsuMask = Mux(isvecH, Cat(Fill(64,1.U),Fill(64,0.U)), Cat(Fill(64,0.U),Fill(64,1.U)) )


  io.out.bits.commits(FuType.alu) := aluOut
  io.out.bits.commits(FuType.lsu) := lsuOut1
  io.out.bits.commits(FuType.csr) := csrOut
  io.out.bits.commits(FuType.mdu) := mduOut
  io.out.bits.commits(FuType.mou) := 0.U
  io.out.bits.commits(FuType.fftu) := fftuOut
  //默认mask是0 原因是reg写入数据不需要mask，只有rvec需要
  io.out.bits.mask := Mux((io.in.bits.ctrl.fuType === FuType.fftu),fftu.io.mask,Mux((isvecL || isvecH),lsuMask,0.U))

  io.in.ready := !io.in.valid || io.out.fire()
//增加在执行级写回的逻辑
  io.forward.valid := io.in.valid
  io.forward.wb.rfWen := io.in.bits.ctrl.rfWen
  io.forward.wb.rfDest := Mux(isFFTreg2vec ,fftu.io.counter_dest ,io.in.bits.ctrl.rfDest)
  io.forward.wb.DestType := io.in.bits.ctrl.DestType
  io.forward.fuType := io.in.bits.ctrl.fuType
  io.forward.wb.rfData := Mux((io.in.bits.ctrl.fuType === FuType.fftu),fftuOut,Mux(alu.io.out.fire(), aluOut, lsuOut1))
  io.forward.wb.mask := Mux((io.in.bits.ctrl.fuType === FuType.fftu),fftu.io.mask,Mux(((isvecL || isvecH)),lsuMask,0.U))

  val isBru = ALUOpType.isBru(fuOpType)
  BoringUtils.addSource(alu.io.out.fire() && !isBru, "perfCntCondMaluInstr")
  BoringUtils.addSource(alu.io.out.fire() && isBru, "perfCntCondMbruInstr")
  BoringUtils.addSource(lsu.io.out.fire(), "perfCntCondMlsuInstr")
  BoringUtils.addSource(mdu.io.out.fire(), "perfCntCondMmduInstr")
  BoringUtils.addSource(csr.io.out.fire(), "perfCntCondMcsrInstr")
  //BoringUtils.addSource(fftu.io.out.fire(), "perfCntCondMfftuInstr")

  if (!p.FPGAPlatform) {
    val mon = Module(new Monitor)
    val cycleCnt = WireInit(0.U(64.W))
    val instrCnt = WireInit(0.U(64.W))
    val nutcoretrap = io.in.bits.ctrl.isNutCoreTrap && io.in.valid
    mon.io.clk := clock
    mon.io.reset := reset.asBool
    mon.io.isNutCoreTrap := nutcoretrap
    mon.io.trapCode := io.in.bits.data.src1
    mon.io.trapPC := io.in.bits.cf.pc
    mon.io.cycleCnt := cycleCnt
    mon.io.instrCnt := instrCnt

    BoringUtils.addSink(cycleCnt, "simCycleCnt")
    BoringUtils.addSink(instrCnt, "simInstrCnt")
    BoringUtils.addSource(nutcoretrap, "nutcoretrap")
  }
}
