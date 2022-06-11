package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import top.Settings

object FFTUOpType{
def set          ="b0000000".U
def test_write1  ="b0000001".U
def test_write2  ="b0000010".U
def test_write3  ="b0000011".U
def test_write4  ="b0000100".U
def test_write5  ="b0000101".U
def test_write6  ="b0000110".U
def test_write7  ="b0000111".U
def test_write8  ="b0001000".U
def complex_mul  ="b0001001".U
def shuffle1     ="b0001010".U
def shuffle2     ="b0001011".U
def shuffle3     ="b0001100".U
def butterfly1   ="b0001101".U
def butterfly2   ="b0001110".U
def butterfly3   ="b0001111".U
}

class FFTUIO extends FunctionUnitIO {
  val cfIn = Flipped(new CtrlFlowIO)
  //val redirect   = new RedirectIO
  val parameters = Input(UInt(VLEN.W))
  val mask      = Output(UInt(VLEN.W))
}

class FFTU extends NutCoreModule{
 val io = IO(new FFTUIO)

 val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
 def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits //用于写回
} 


val twiddle = Reg(Vec(24,UInt(32.W)))
// twiddle  for  layer 1 
twiddle(0) := "b00000000_00000000_00000001_00000000".U 
twiddle(1) := "b00000000_00000000_00000001_00000000".U  
twiddle(2) := "b00000000_00000000_00000001_00000000".U 
twiddle(3) := "b00000000_00000000_00000001_00000000".U 
twiddle(4) := "b00000000_00000000_00000001_00000000".U 
twiddle(5) := "b00000000_00000000_00000001_00000000".U 
twiddle(6) := "b00000000_00000000_00000001_00000000".U 
twiddle(7) := "b00000000_00000000_00000001_00000000".U 
// twiddle  for  layer  2
twiddle(8) := "b00000000_00000000_00000001_00000000".U 
twiddle(9) := "b00000000_00000000_00000001_00000000".U 
twiddle(10):= "b00000000_00000000_00000001_00000000".U 
twiddle(11):= "b11111111_00000000_00000000_00000000".U 
twiddle(12):= "b00000000_00000000_00000001_00000000".U 
twiddle(13):= "b00000000_00000000_00000001_00000000".U 
twiddle(14):= "b00000000_00000000_00000001_00000000".U 
twiddle(15):= "b11111111_00000000_00000000_00000000".U 
// twiddle  for  layer   3
twiddle(16):= "b00000000_00000000_00000001_00000000".U 
twiddle(17):= "b00000000_00000000_00000001_00000000".U 
twiddle(18):= "b00000000_00000000_00000001_00000000".U 
twiddle(19):= "b00000000_00000000_00000001_00000000".U 

twiddle(20):= "b00000000_00000000_00000001_00000000".U 
twiddle(21):= "b11111111_01001011_00000000_10110101".U 
twiddle(22):= "b11111111_00000000_00000000_00000000".U 
twiddle(23):= "b11111111_01001011_11111111_01001011".U 



val Data_len = 8 
var Num_max  = 8

val mid_result = Wire(Vec((16), UInt(128.W)))
val result     = WireInit(0.U(128.W))

val data_num = RegInit(0.U(4.W))
val data_len = RegInit(0.U(4.W))

val layer  = Wire(UInt(2.W))
val start  = Wire(UInt(3.W))
val num    = WireInit(0.U(4.W))

layer := io.parameters(5,4)
start := io.parameters(9,7)
mid_result  := VecInit((0 to 15).map(i => 0.U))




/* Operation for Set */
when(func === FFTUOpType.set){
  data_num:= MuxCase(0.U, 
  Array(
  (io.parameters(4)===1.U) -> 2.U,
  (io.parameters(5)===1.U) -> 4.U,
  (io.parameters(6)===1.U) -> 8.U
))

  num:= MuxCase(0.U, 
  Array(
  (io.parameters(4)===1.U) -> 2.U,
  (io.parameters(5)===1.U) -> 4.U,
  (io.parameters(6)===1.U) -> 8.U
))
}

/* Operation for Copmlex_Mul */
when(func === FFTUOpType.complex_mul){
mid_result(0) := src1
var k = 0
for (i <- 1 to (Num_max-1)){

k=k+1
val setnum = 1.U<<layer
val picknum = 1.U<<(layer-1.U)

when((i.U % setnum)>=picknum && i.U < data_num){

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

//k=1,3,5,7
tr1:= twiddle(k)(15,0)
ti1:= twiddle(k)(31,16)
//k=2,3,6,7
tr2:= twiddle(k+8)(15,0)
ti2:= twiddle(k+8)(31,16)
//k=4,5,6,7
tr3:= twiddle(k+16)(15,0)
ti3:= twiddle(k+16)(31,16)


when(layer===1.U){
real  := (c*tr1)-(d*ti1)
image := (c*ti1)+(d*tr1)
}.elsewhen(layer===2.U){
real  := (c*tr2)-(d*ti2)
image := (c*ti2)+(d*tr2)
}.elsewhen(layer===3.U){
real  := (c*tr3)-(d*ti3)
image := (c*ti3)+(d*tr3)
}

printf("layer is %b, k is %b, c is %b, d is %b, tr3 is %b, ti3 is %b\n real is %b, image is %b \n",layer,k.U,c,d,tr3,ti3,real,image)

if(i == 7)
{
mid_result(k) := Cat(image(15,8),real(15,8),mid_result(k-1)(i*2*Data_len-1,0))  }
else{
mid_result(k) := Cat(mid_result(k-1)(127,(2*i+2)*Data_len),image(15,8),real(15,8),mid_result(k-1)(i*2*Data_len-1,0))
} 

}.otherwise{
 
mid_result(k) := mid_result(k-1)
}
result := mid_result(k)
}
}





/* Operation for Butterfly   */
/* when(func === FFTUOpType.butterfly1){
  var j =0
  for(i <- 0 to (Num_max-1) by 2) {
  j=j+1

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
  

  butterfly(j) := ZeroExt( Cat(d,c,b,a),128)
  mid_result(j) := mid_result(j-1) | (butterfly(j) << (2*i*Data_len).U)
  result := mid_result(j)

}
}

 */


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
}}



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
}}




/* Operation for Shuffle   */
when(func === FFTUOpType.shuffle1)
{
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
 

when(func === FFTUOpType.shuffle2)
{
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
 
when(func === FFTUOpType.shuffle3)
{
  for (i <- 1 to 3)
  {
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
 




//输出信息打印
/* when(func === FFTUOpType.set){
printf("SET DATA NUM is %x",num)
}  */
when(func === FFTUOpType.complex_mul){
printf("complex_mul src1 is %x, \n",src1)
printf("complex_mul result is %x \n",result)
} 

when(func === FFTUOpType.butterfly1 ){
printf("Butterfly1 src1 is %x, the result is %x \n",src1, result)
} 
when(func === FFTUOpType.butterfly2 ){
printf("Butterfly2 src1 is %x, the result is %x \n",src1, result)
} 
when(func === FFTUOpType.butterfly3){
printf("Butterfly3 src1 is %x, the result is %x \n",src1, result)
} 
when(func === FFTUOpType.shuffle1){
printf("Shuffle1 src1 is %x, the result is %x \n",src1, result)
} 
when(func === FFTUOpType.shuffle2){
printf("Shuffle2 src1 is %x, the result is %x \n",src1, result)
} 
when(func === FFTUOpType.shuffle3){
printf("Shuffle3 src1 is %x, the result is %x \n",src1, result)
} 

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
(func === FFTUOpType.shuffle1)     -> Cat(Fill(128,1.U)),
(func === FFTUOpType.shuffle2)     -> Cat(Fill(128,1.U)),
(func === FFTUOpType.shuffle3)     -> Cat(Fill(128,1.U)),
(func === FFTUOpType.complex_mul)  -> Cat(Fill(128,1.U))
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
(func === FFTUOpType.shuffle1)     -> result,
(func === FFTUOpType.shuffle2)     -> result,
(func === FFTUOpType.shuffle3)     -> result,
(func === FFTUOpType.complex_mul)  -> result

))

io.in.ready := io.out.ready
io.out.valid := valid


}