package nutcore
import java.io._
import chisel3._
import chisel3.util._
import chisel3.experimental._
import scala.math._
import chisel3.util.experimental.BoringUtils

import utils._
import top.Settings

object FFTUOpType{
 def setLength            ="b0000000".U
 def test_write1          ="b0000001".U
 def test_write2          ="b0000010".U
 def test_write3          ="b0000011".U
 def test_write4          ="b0000100".U
 def test_write5          ="b0000101".U
 def test_write6          ="b0000110".U
 def test_write7          ="b0000111".U
 def test_write8          ="b0001000".U
 def complex_mul          ="b0001001".U
 def shuffle_single       ="b0001010".U
 def shuffle_double0      ="b0001011".U
 def shuffle_double1      ="b0001100".U
 def butterfly_single0    ="b0001101".U
 def butterfly_single1    ="b0001110".U
 def butterfly_double     ="b0010000".U
 def write_buffer         ="b0010001".U
 def reg2vec              ="b0010010".U
 def vecl2reg             ="b0010011".U
 def vech2reg             ="b0010100".U
 def clear_counter        ="b0010101".U
 def clear_src1           ="b0010110".U
}

class FFTUIO extends FunctionUnitIO {
  val cfIn = Flipped(new CtrlFlowIO)
  val parameters = Input(UInt(VLEN.W))
  val mask       = Output(UInt(VLEN.W))
  val counter_dest = Output(UInt(5.W))
}


class FFTU extends NutCoreModule with Twiddle {
  val io = IO(new FFTUIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits //用于写回
} 

/******************************  Signal Defination  *****************************************/
  val pc_reg      = RegInit(0.U)  
  val result      = WireInit(0.U(128.W))
  val result_buff_reg = RegInit(0.U(128.W))
  pc_reg := io.cfIn.pc

  //val mid_result  = Wire(Vec((16), UInt(128.W)))
  //val result_buff = Wire(Vec((16), UInt(128.W)))
  //val data_num = RegInit(4.U(3.W)) //In case for not set
  //val layer  = WireInit(0.U(3.W))  // Only used for comlex_mul inst
  //val start  = WireInit(0.U(3.W))  // Only used for Shuffle inst
  //val Data_len = 16 //for real 16bits and image 16bits
  //val Num_max  = 4  //4*(16+16) = 128
  //layer := io.parameters(6,4) 
  //start := io.parameters(15,12)
  //mid_result   := VecInit((0 to 15).map(i => 0.U))
  //result_buff  := VecInit((0 to 15).map(i => 0.U))

  




/******************************  Inst Operation  *****************************************/

  /* Operation for Set */
  //Set how many data need to use in a RVec




  // when(func === FFTUOpType.setLength){
  //   FFTLength = pow(2,10).toInt.asUInt
  // } 




/* Regular Register to Vec Register */
  val data_trans_counter = RegInit(0.U(5.W))

    when(func === FFTUOpType.reg2vec){
      when(pc_reg =/= io.cfIn.pc){
        data_trans_counter:= data_trans_counter + 1.U
        }
      result:= src1<<64 | src2
    }
  
    when(func === FFTUOpType.vech2reg){
      when(pc_reg =/= io.cfIn.pc){
        data_trans_counter:= data_trans_counter + 1.U
        }
      result:= Cat(Fill(64, 0.U) ,src1(63,0))
    }

    when(func === FFTUOpType.vecl2reg){
      when(pc_reg =/= io.cfIn.pc){
        data_trans_counter:= data_trans_counter + 1.U
        }
      result:= Cat(Fill(64, 0.U) ,src1(127,64))
    }
  
    when(func === FFTUOpType.clear_counter){
      data_trans_counter:= 0.U
    }

  io.counter_dest := data_trans_counter


 val a = RegInit(3.U)

/* Operation for Copmlex_Mul */
  when(func === FFTUOpType.complex_mul){
    
    //Generate WnTable
    var k = 0
    val wn0,wn1,wn2,wn3 = RegInit(0.U)
    for(k <- 0 to 10){
      when(k.U === a){
       wn0 := wnTable(k)(a).asUInt
       wn1 := wnTable(k)(a).asUInt
       wn2 := wnTable(k)(a).asUInt
       wn3 := wnTable(k)(a).asUInt
      }
    }
   

    val num0 = src1(31,0)
    val num1 = src1(63,32)
    val num2 = src1(95,64)
    val num3 = src1(127,96)
  
    val new_num0 = complex_multiplier(num0,wn0)
    val new_num1 = complex_multiplier(num1,wn1)
    val new_num2 = complex_multiplier(num2,wn2)
    val new_num3 = complex_multiplier(num3,wn3)

    result := Cat(new_num3,new_num2,new_num1,new_num0)
    
    }


  when(func === FFTUOpType.butterfly_single0){
     val num0 = src1(31,0)
     val num1 = src1(63,32)
     val num2 = src1(95,64)
     val num3 = src1(127,96)

     val new_num0 = complex_add(num0 ,num1)
     val new_num1 = complex_sub(num0 ,num1)
     val new_num2 = complex_add(num2 ,num3)
     val new_num3 = complex_sub(num2 ,num3)
     
     result := Cat(new_num3,new_num2,new_num1,new_num0)
  }

  when(func === FFTUOpType.butterfly_single1){
     val num0 = src1(31,0)
     val num1 = src1(63,32)
     val num2 = src1(95,64)
     val num3 = src1(127,96)

     val new_num0 = complex_add(num0 ,num2)
     val new_num2 = complex_sub(num0 ,num2)
     val new_num1 = complex_add(num1 ,num3)
     val new_num3 = complex_sub(num1 ,num3)
     
     result := Cat(new_num3,new_num2,new_num1,new_num0)
  }

    when(func === FFTUOpType.butterfly_double){
     val v1n0 = src1(31,0)
     val v1n1 = src1(63,32)
     val v1n2 = src1(95,64)
     val v1n3 = src1(127,96)
     val v2n0 = src2(31,0)
     val v2n1 = src2(63,32)
     val v2n2 = src2(95,64)
     val v2n3 = src2(127,96)

     val new_v1n0 = complex_add(v1n0,v2n0)
     val new_v2n0 = complex_sub(v1n0,v2n0)

     val new_v1n1 = complex_add(v1n1,v2n1)
     val new_v2n1 = complex_sub(v1n1,v2n1)

     val new_v1n2 = complex_add(v1n2,v2n2)
     val new_v2n2 = complex_sub(v1n2,v2n2)

     val new_v1n3 = complex_add(v1n3,v2n3)
     val new_v2n3 = complex_sub(v1n3,v2n3)

     result          := Cat(new_v1n3,new_v1n2,new_v1n1,new_v1n0)
     result_buff_reg := Cat(new_v2n3,new_v2n2,new_v2n1,new_v2n0)
  }


  when(func === FFTUOpType.write_buffer){
    result := result_buff_reg
    result_buff_reg := 0.U


  //  val sinn = sinTable(1)
  //  for(i <- 0 until 8){
  //   printf("sintable is = %x\n",sinn(i).asSInt)
  //  }
}


  when(func === FFTUOpType.shuffle_double0){
       val src1_num0  = src1(31,0)
       val src1_num1  = src1(63,32)
       val src1_num2  = src1(95,64)
       val src1_num3  = src1(127,96)

       val src2_num0  = src2(31,0)
       val src2_num1  = src2(63,32)
       val src2_num2  = src2(95,64)
       val src2_num3  = src2(127,96)

       result          := Cat(src2_num2,src1_num2,src2_num0,src1_num0)
       result_buff_reg := Cat(src2_num3,src1_num3,src2_num1,src1_num1)

  }

    when(func === FFTUOpType.shuffle_double1){
       val src1_num0  = src1(31,0)
       val src1_num1  = src1(63,32)
       val src1_num2  = src1(95,64)
       val src1_num3  = src1(127,96)

       val src2_num0  = src2(31,0)
       val src2_num1  = src2(63,32)
       val src2_num2  = src2(95,64)
       val src2_num3  = src2(127,96)

       result          := Cat(src2_num1,src2_num0,src1_num1,src1_num0)
       result_buff_reg := Cat(src2_num3,src2_num2,src1_num3,src1_num2)
  }

    when(func === FFTUOpType.shuffle_single){
       val src1_num0  = src1(31,0)
       val src1_num1  = src1(63,32)
       val src1_num2  = src1(95,64)
       val src1_num3  = src1(127,96)

       result := Cat(src1_num3,src1_num1,src1_num2,src1_num0)
  }



/******************************  OutPut *****************************************/




  io.mask := MuxCase( Cat(Fill(128, 0.U)), Array(
  (func === FFTUOpType.test_write1)  -> Cat(Fill(112, 0.U),Fill(16,1.U)), 
  (func === FFTUOpType.test_write2)  -> Cat(Fill(96, 0.U),Fill(16,1.U),Fill(16,0.U)),
  (func === FFTUOpType.test_write3)  -> Cat(Fill(80, 0.U),Fill(16,1.U),Fill(32,0.U)),
  (func === FFTUOpType.test_write4)  -> Cat(Fill(64, 0.U),Fill(16,1.U),Fill(48,0.U)),
  (func === FFTUOpType.test_write5)  -> Cat(Fill(48, 0.U),Fill(16,1.U),Fill(64,0.U)),
  (func === FFTUOpType.test_write6)  -> Cat(Fill(32, 0.U),Fill(16,1.U),Fill(80,0.U)),
  (func === FFTUOpType.test_write7)  -> Cat(Fill(16, 0.U),Fill(16,1.U),Fill(96,0.U)),
  (func === FFTUOpType.test_write8)  -> Cat(Fill(16, 1.U),Fill(112,0.U)),
  (func === FFTUOpType.butterfly_single0)   -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.butterfly_single1)   -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.butterfly_double)    -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.write_buffer)        -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.shuffle_single)      -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.shuffle_double0)     -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.shuffle_double1)     -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.complex_mul)         -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.reg2vec)             -> Cat(Fill(128,1.U))
  ))


  io.out.bits := MuxCase(result, Array(
  (func === FFTUOpType.test_write1) -> Cat(Fill(112, 0.U),io.parameters(15,0)), 
  (func === FFTUOpType.test_write2) -> Cat(Fill(96, 0.U) ,io.parameters(15,0),Fill(16,0.U)),
  (func === FFTUOpType.test_write3) -> Cat(Fill(80, 0.U) ,io.parameters(15,0),Fill(32,0.U)),
  (func === FFTUOpType.test_write4) -> Cat(Fill(64, 0.U) ,io.parameters(15,0),Fill(48,0.U)),
  (func === FFTUOpType.test_write5) -> Cat(Fill(48, 0.U) ,io.parameters(15,0),Fill(64,0.U)),
  (func === FFTUOpType.test_write6) -> Cat(Fill(32, 0.U) ,io.parameters(15,0),Fill(80,0.U)),
  (func === FFTUOpType.test_write7) -> Cat(Fill(16, 0.U) ,io.parameters(15,0),Fill(96,0.U)),
  (func === FFTUOpType.test_write8) -> Cat(io.parameters(15,0),Fill(112, 0.U))
  // (func === FFTUOpType.butterfly_single0)   -> result,
  // (func === FFTUOpType.butterfly_single1)   -> result,
  // (func === FFTUOpType.butterfly_double)    -> result,
  // (func === FFTUOpType.write_buffer)        -> result,
  // (func === FFTUOpType.shuffle_single)      -> result,
  // (func === FFTUOpType.shuffle_double0)     -> result,
  // (func === FFTUOpType.shuffle_double1)     -> result,
  // (func === FFTUOpType.complex_mul)         -> result,
  // (func === FFTUOpType.reg2vec)             -> result,
  // (func === FFTUOpType.vecl2reg)            -> result,
  // (func === FFTUOpType.vech2reg)            -> result

  ))

  io.in.ready  := io.out.ready
  io.out.valid := valid


  val data = List("ok for nemu test", "you", "want", "to", "write", "to", "the", "file") 
  val file = "/data/jsj/proj/fftu_result.txt"
  val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
  for (x <- data) {
    writer.write(x + "\n")  // however you want to format it
  }
  writer.close()





}