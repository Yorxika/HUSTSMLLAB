(*sum:int list->int*)
(*REQUIRES:true    *)
(*ENSURES:sum(L) evaluates to the sum of the integers in L.*)
fun sum[]=0
  |sum(x::L)=x+(sum L);
  
(*mult:int list->int*)
(*REQUIRES:true    *)
(*ENSURES:mult(L) evaluates to the product of the integers in L.*)
fun mult[]=1
  |mult(x::L)=x*(mult L);
  
(*Mult:int (int list) list->int *)
(*REQUIRES:true    *)
(*ENSURES:Mult(R) evaluates to the product of all the integers in the list of R.*)
fun Mult[]=1
  |Mult(r::R)=mult(r)*Mult(R);
  
(*mult':int list * int ->int *)
(*REQUIRES:true    *)
(*ENSURES:求组里int和list里的所有元素的乘积*)  
fun mult'([],a)=a
  |mult'(x::L,a)=mult'(L,x*a);
  
fun Mult'([],a)=a
  |Mult'(r::R,a)=Mult(R)*mult'(r,a);
  
fun double(0:int):int=0
  |double n=2+double(n-1);

fun square(0:int):int=0
  | square x=square(x-1)+double(x)-1;
  
fun divisibleByThree(0:int):bool = true
   | divisibleByThree 1 = false
   | divisibleByThree 2 = false
   | divisibleByThree n = divisibleByThree(n-3);
   
fun oddp(0:int):bool = false
   | oddp 1 = true
   | oddp n = oddp(n-2);
