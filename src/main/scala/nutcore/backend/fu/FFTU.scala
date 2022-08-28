package nutcore
import java.io._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import top.Settings

object FFTUOpType{
 def set              ="b0000000".U
 def test_write1      ="b0000001".U
 def test_write2      ="b0000010".U
 def test_write3      ="b0000011".U
 def test_write4      ="b0000100".U
 def test_write5      ="b0000101".U
 def test_write6      ="b0000110".U
 def test_write7      ="b0000111".U
 def test_write8      ="b0001000".U
 def complex_mul      ="b0001001".U
 def shuffle1         ="b0001010".U
 def shuffle2         ="b0001011".U
 def shuffle3         ="b0001100".U
 def butterfly1       ="b0001101".U
 def butterfly2       ="b0001110".U
 def butterfly3       ="b0001111".U
 def butterfly0       ="b0010000".U
 def butterfly00      ="b0010001".U
 def reg2vec          ="b0010010".U
 def vecl2reg         ="b0010011".U
 def vech2reg         ="b0010100".U
 def clear_counter    ="b0010101".U
 def clear_src1       ="b0010110".U
}

class FFTUIO extends FunctionUnitIO {
  val cfIn = Flipped(new CtrlFlowIO)
  //val redirect   = new RedirectIO
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


  val pc_reg = RegInit(0.U)
  pc_reg := io.cfIn.pc

  val Data_len = 8 //for real 16bits and image 16bits
  val Num_max  = 8

  val result      = WireInit(0.U(128.W))
  val mid_result  = Wire(Vec((16), UInt(128.W)))
  val result_buff = Wire(Vec((16), UInt(128.W)))
  val result_buff_reg = RegInit(0.U(128.W))

  val data_num = RegInit(8.U(4.W))
  //val data_len = RegInit(0.U(4.W)) 
  

  val layer  = Wire(UInt(3.W))// Only used for comlex_mul inst
  val start  = Wire(UInt(3.W))// Only used for Shuffle inst
  //parameters = inst(16,31)
  layer := io.parameters(6,4) 
  start := io.parameters(9,7)
  mid_result   := VecInit((0 to 15).map(i => 0.U))
  result_buff  := VecInit((0 to 15).map(i => 0.U))


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

  /* Operation for Set */
  //Set how many data need to use in a RVec
  when(func === FFTUOpType.set){
    data_num:= MuxCase(0.U, 
    Array(
      (io.parameters(4)===1.U) -> 2.U,
      (io.parameters(5)===1.U) -> 4.U,
      (io.parameters(6)===1.U) -> 8.U))
  }



  /* Operation for Copmlex_Mul */
  when(func === FFTUOpType.complex_mul){
   mid_result(0) := src1
   var k = 0

      for (i <- 0 to (Num_max-1)){
      
      //variable to Control For-Loop 
      val setnum  = WireInit(0.U(4.W))
      val picknum = WireInit(0.U(4.W))
        when(layer<4.U){
        setnum := 1.U<<layer
        picknum := 1.U<<(layer-1.U)
        }.otherwise{  //used for multiply all 8 num in RVec
        setnum := 2.U
        picknum := 0.U
        }

      //For-Loop to Control Complex_Mul
        when((i.U % setnum)>=picknum && i.U < data_num){
          //16bits * 16bits complex 
            val a = WireInit(0.U(Data_len.W))
            val b = WireInit(0.U(Data_len.W))
            a := src1((2*i+1)*Data_len-1,i*2*Data_len)//实部
            b := src1((2*i+2)*Data_len-1,(2*i+1)*Data_len)//虚部

            val c = Cat(Fill(8,a(Data_len-1)),a)  
            val d = Cat(Fill(8,b(Data_len-1)),b)

            val real  = WireInit(0.U(16.W)) 
            val image = WireInit(0.U(16.W))

            val tr1 = WireInit(0.U(16.W))
            val ti1 = WireInit(0.U(16.W))
            val tr2 = WireInit(0.U(16.W))
            val ti2 = WireInit(0.U(16.W))
            val tr3 = WireInit(0.U(16.W))
            val ti3 = WireInit(0.U(16.W))
            val tr4 = WireInit(0.U(16.W))
            val ti4 = WireInit(0.U(16.W))

            //k=1,3,5,7
            tr1:= twiddle(k)(15,0)
            ti1:= twiddle(k)(31,16)
            //k=2,3,6,7
            tr2:= twiddle(k+8)(15,0)
            ti2:= twiddle(k+8)(31,16)
            //k=4,5,6,7
            tr3:= twiddle(k+16)(15,0)
            ti3:= twiddle(k+16)(31,16)
            //
            tr4:= twiddle(k+24)(15,0)
            ti4:= twiddle(k+24)(31,16)

            when(layer===1.U){
            real  := (c*tr1)-(d*ti1)
            image := (c*ti1)+(d*tr1)
            }.elsewhen(layer===2.U){
            real  := (c*tr2)-(d*ti2)
            image := (c*ti2)+(d*tr2)
            }.elsewhen(layer===3.U){
            real  := (c*tr3)-(d*ti3)
            image := (c*ti3)+(d*tr3)
            }.elsewhen(layer===4.U){
            real  := (c*tr4)-(d*ti4)
            image := (c*ti4)+(d*tr4)
            }

            //printf("layer is %b, k is %b, c is %b, d is %b, tr3 is %b, ti3 is %b\n real is %b, image is %b \n",layer,k.U,c,d,tr3,ti3,real,image)
            if(i == 0)
            { mid_result(0) := Cat(image(15,8),real(15,8))}
            else if(i == 7)
            { mid_result(k) := Cat(image(15,8),real(15,8),mid_result(k-1)(i*2*Data_len-1,0)) }
            else{ 
              mid_result(k) := Cat(mid_result(k-1)(127,(2*i+2)*Data_len),image(15,8),real(15,8),mid_result(k-1)(i*2*Data_len-1,0)) } 

        }.otherwise{ 
          if(k==0){mid_result(k) :=0.U}
          else if(k>0){mid_result(k) := mid_result(k-1) }
        }
      result := mid_result(k)
      k=k+1
      }
  }


  when(func === FFTUOpType.butterfly1){
    var j =0
    for(i <- 0 to (Num_max-2) ) {
      j=j+1
      val setnum  = 2.U
        when(((i.U % setnum) === 0.U || i.U===0.U) && i.U < data_num ){
          val num1r = src1(16*i+7,16*i)
          val num1i = src1(16*i+15,16*i+8)
          val num2r = src1(16*i+23,16*i+16)
          val num2i = src1(16*i+31,16*i+24)
          val a = WireInit(0.U(8.W))
          val b = WireInit(0.U(8.W))
          val c = WireInit(0.U(8.W))
          val d = WireInit(0.U(8.W))
          a:= num1r+num2r
          b:= num1i+num2i
          c:= num1r-num2r
          d:= num1i-num2i


          mid_result(j) := Cat(d,c,b,a)<<(16*i).U | mid_result(j-1)

        }.otherwise{
          mid_result(j) := mid_result(j-1)
        }
      result := mid_result(j)
    }
  }

  when(func === FFTUOpType.butterfly2){
    var j = 0  
      for(i <- 0 to (Num_max-3) ) {
        j=j+1

        val setnum  = 4.U
        val picknum = 2.U

        when( ((i.U % setnum) === 0.U || (i.U % setnum) === 1.U)&& i.U < data_num ){
        
          val num1r = src1(16*i+7,16*i)
          val num1i = src1(16*i+15,16*i+8)
          val num2r = src1(16*(i+2)+7,16*(i+2))
          val num2i = src1(16*(i+2)+15,16*(i+2)+8)  

          val a = WireInit(0.U(8.W))
          val b = WireInit(0.U(8.W))
          val c = WireInit(0.U(8.W))
          val d = WireInit(0.U(8.W))
          a:= num1r+num2r
          b:= num1i+num2i
          c:= num1r-num2r
          d:= num1i-num2i

          mid_result(j) := Cat(b,a)<<(16*i).U|Cat(d,c)<<(16*(i+2)).U | mid_result(j-1)

        }.otherwise{
          mid_result(j) := mid_result(j-1)
        }
       result := mid_result(j)
    }
  }

  when(func === FFTUOpType.butterfly3){
    var j = 0
    for(i <- 0 to (Num_max-5) ) {
      j = j+1
      val setnum  = 8.U
      val picknum = 4.U

      when( (i.U % setnum) < picknum && i.U < data_num  ){
      
        val num1r = src1(16*i+7,16*i)
        val num1i = src1(16*i+15,16*i+8)
        val num2r = src1(16*(i+4)+7,16*(i+4))
        val num2i = src1(16*(i+4)+15,16*(i+4)+8)  

        val a = WireInit(0.U(8.W))
        val b = WireInit(0.U(8.W))
        val c = WireInit(0.U(8.W))
        val d = WireInit(0.U(8.W))
        a:= num1r+num2r
        b:= num1i+num2i
        c:= num1r-num2r
        d:= num1i-num2i

        mid_result(j) := Cat(b,a)<<(16*i).U|Cat(d,c)<<(16*(i+4)).U | mid_result(j-1)

      }.otherwise{
        mid_result(j) := mid_result(j-1)
      }
      result := mid_result(j)
    }
  }

  // ADD For more than 8 data-nums
  when(func === FFTUOpType.butterfly0){
    var j = 0
      for(i <- 0 to (Num_max-1) ) {
        j = j+1

        when(i.U < data_num ){
        
          val num1r = src1(16*i+7,16*i)
          val num1i = src1(16*i+15,16*i+8)
          val num2r = src2(16*i+7,16*i)
          val num2i = src2(16*i+15,16*i+8)


          val a = WireInit(0.U(8.W))
          val b = WireInit(0.U(8.W))
          val c = WireInit(0.U(8.W))
          val d = WireInit(0.U(8.W))
          a:= num1r+num2r
          b:= num1i+num2i
          c:= num1r-num2r
          d:= num1i-num2i

          mid_result(j)  := Cat(b,a)<<(16*i).U| mid_result(j-1)
          result_buff(j) := Cat(d,c)<<(16*i).U| result_buff(j-1)

        }.otherwise{
          mid_result(j)  := mid_result(j-1)
          result_buff(j) := result_buff(j-1)
          }
        result := mid_result(j)
        result_buff_reg:=result_buff(j)
    }
  }
  //Use together with butterfly0
  when(func === FFTUOpType.butterfly00){
    result := result_buff_reg
    result_buff_reg := 0.U

  }



  /* Operation for Shuffle   */
  when(func === FFTUOpType.shuffle1){
    for (i <- 1 to 5)
    {
      when (i.U === start){
       val j = i+1
       val front = src1(i*2*Data_len-1,0)
       val num1  = src1((i+1)*2*Data_len-1,i*2*Data_len)
       val num2  = src1((j+1)*2*Data_len-1,j*2*Data_len)
       val back  = src1(127,(j+1)*2*Data_len)

       result := Cat(back,num1,num2,front) 
      }

    }
  }


  when(func === FFTUOpType.shuffle2){
    for (i <- 1 to 4)
    {
      when (i.U === start){
       val j = i+2
       val front = src1(i*2*Data_len-1,0)
       val num1  = src1((i+1)*2*Data_len-1,i*2*Data_len)
       val mid   = src1(j*2*Data_len-1,(i+1)*2*Data_len)
       val num2  = src1((j+1)*2*Data_len-1,j*2*Data_len)
       val back  = src1(127,(j+1)*2*Data_len)

       result := Cat(back,num1,mid,num2,front) 
      }

    }
  }

  when(func === FFTUOpType.shuffle3){
    for (i <- 1 to 3){
      when (i.U === start){
       val j = i+3
       val front = src1(i*2*Data_len-1,0)
       val num1  = src1((i+1)*2*Data_len-1,i*2*Data_len)
       val mid   = src1(j*2*Data_len-1,(i+1)*2*Data_len)
       val num2  = src1((j+1)*2*Data_len-1,j*2*Data_len)
       val back  = src1(127,(j+1)*2*Data_len)

       result := Cat(back,num1,mid,num2,front) 
      //result := Cat(src1(127,(j+1)*2*Data_len),src1((j+1)*2*Data_len-1,j*2*Data_len),src1((i+1)*2*Data_len-1,i*2*Data_len),src1(j*2*Data_len-1,(i+1)*2*Data_len),src1(i*2*Data_len-1,0))
      }

    }
  }



//输出到文件

val data = List("ok for nemu test", "you", "want", "to", "write", "to", "the", "file")
val file = "/data/jsj/proj/fftu_result.txt"
val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
for (x <- data) {
  writer.write(x + "\n")  // however you want to format it
}
writer.close()





  //产生输出信号
  io.mask := MuxCase( Cat(Fill(128, 0.U)), Array(
  (func === FFTUOpType.test_write1)  -> Cat(Fill(112, 0.U),Fill(16,1.U)), 
  (func === FFTUOpType.test_write2)  -> Cat(Fill(96, 0.U),Fill(16,1.U),Fill(16,0.U)),
  (func === FFTUOpType.test_write3)  -> Cat(Fill(80, 0.U),Fill(16,1.U),Fill(32,0.U)),
  (func === FFTUOpType.test_write4)  -> Cat(Fill(64, 0.U),Fill(16,1.U),Fill(48,0.U)),
  (func === FFTUOpType.test_write5)  -> Cat(Fill(48, 0.U),Fill(16,1.U),Fill(64,0.U)),
  (func === FFTUOpType.test_write6)  -> Cat(Fill(32, 0.U),Fill(16,1.U),Fill(80,0.U)),
  (func === FFTUOpType.test_write7)  -> Cat(Fill(16, 0.U),Fill(16,1.U),Fill(96,0.U)),
  (func === FFTUOpType.test_write8)  -> Cat(Fill(16, 1.U),Fill(112,0.U)),
  (func === FFTUOpType.butterfly1)   -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.butterfly2)   -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.butterfly3)   -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.butterfly0)   -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.butterfly00)  -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.shuffle1)     -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.shuffle2)     -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.shuffle3)     -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.complex_mul)  -> Cat(Fill(128,1.U)),
  (func === FFTUOpType.reg2vec)      -> Cat(Fill(128,1.U))
  ))


  io.out.bits := MuxCase( Cat(Fill(128, 0.U)), Array(
  (func === FFTUOpType.test_write1) -> Cat(Fill(112, 0.U),io.parameters(15,0)), 
  (func === FFTUOpType.test_write2) -> Cat(Fill(96, 0.U) ,io.parameters(15,0),Fill(16,0.U)),
  (func === FFTUOpType.test_write3) -> Cat(Fill(80, 0.U) ,io.parameters(15,0),Fill(32,0.U)),
  (func === FFTUOpType.test_write4) -> Cat(Fill(64, 0.U) ,io.parameters(15,0),Fill(48,0.U)),
  (func === FFTUOpType.test_write5) -> Cat(Fill(48, 0.U) ,io.parameters(15,0),Fill(64,0.U)),
  (func === FFTUOpType.test_write6) -> Cat(Fill(32, 0.U) ,io.parameters(15,0),Fill(80,0.U)),
  (func === FFTUOpType.test_write7) -> Cat(Fill(16, 0.U) ,io.parameters(15,0),Fill(96,0.U)),
  (func === FFTUOpType.test_write8) -> Cat(io.parameters(15,0),Fill(112, 0.U)),
  (func === FFTUOpType.butterfly1)   -> result,
  (func === FFTUOpType.butterfly2)   -> result,
  (func === FFTUOpType.butterfly3)   -> result,
  (func === FFTUOpType.butterfly0)   -> result,
  (func === FFTUOpType.butterfly00)  -> result,
  (func === FFTUOpType.shuffle1)     -> result,
  (func === FFTUOpType.shuffle2)     -> result,
  (func === FFTUOpType.shuffle3)     -> result,
  (func === FFTUOpType.complex_mul)  -> result,
  (func === FFTUOpType.reg2vec)      -> result,
  (func === FFTUOpType.vecl2reg)     -> result,
  (func === FFTUOpType.vech2reg)     -> result

  ))

  io.in.ready  := io.out.ready
  io.out.valid := valid


}