fun douBle(x : int) : int = 2*x;
fun square(x : int) : int = x*x;
fun jiecheng(1 , x) : int = x
  | jiecheng(m , x) = jiecheng(m - 1 , m * x);
fun fact(x : int) : int = jiecheng(x , 1);

fun thenAddOne(f , x) = f x + 1;

fun maplist(f,l) = 
    case l of
        [] => []
        |(x::L) => (f x)::maplist(f,L);
  
fun maplist'(f) = 
    let 
        fun temp[] = []
        |temp(x::L) = f x::temp(L)
    in temp
    end

fun findOdd([] : int list): int option = NONE
  | findOdd(x::L) = if (x mod 2)=1 then SOME x else findOdd L							  
  
fun subsetSumOption (l : int list, 0 : int) : int list option = SOME []
  | subsetSumOption ([], sum) = NONE
  | subsetSumOption (x::L, sum) =
  if subsetSumOption(L, sum-x) = NONE 
  then subsetSumOption(L, sum) 
  else
  SOME (x::(valOf(subsetSumOption(L, sum-x))))
	
fun exists(_,[]) = false
  | exists(p,x::L) = if p x = true then true else exists(p,L);
  
fun forall(_,[]) = false
  | forall(p,[x]) = p x
  | forall(p,x::L) = p x andalso forall(p,L);
   
fun isOdd(x) = if(x mod 2) = 1 then true else false;

	
datatype 'a tree = Empty
    | Node of 'a tree * 'a * 'a tree;

fun listToTree [] = Empty 
  | listToTree (x::L) = 
	let 
      val k = (length L)div 2
    in 
      Node(listToTree(List.take(L,k)),x,listToTree(List.drop(L,k)))
    end;

fun treeFilter(_,Empty) = Empty
  | treeFilter(p,Node(l,x,r)) = 
	if p x = true then 
		Node(treeFilter(p,l),SOME(x),treeFilter(p,r))
    else
        Node(treeFilter(p,l),NONE,treeFilter(p,r));
	
(* 以下为测试用例 *)
	
val tmp_list = [1,2,3,4,5,6,7];
val testThenAddOneDouble = thenAddOne(douBle,3);
val testThenAddOneSquare = thenAddOne(square,3);
val testThenAddOneFact = thenAddOne(fact,3);

val testMaplist = maplist(fact,tmp_list);

val testMaplist' = maplist'(square)tmp_list;

val testFindOdd = findOdd(tmp_list);

val testSubSetSumOption = subsetSumOption(tmp_list,10);

val testExistsOdd = exists(isOdd,tmp_list);
val testExistsOdd2 = exists(isOdd,[2,4,6]);
val testForallOdd = forall(isOdd,tmp_list);
val testForallOdd = forall(isOdd,[1,3,5]);

val testTreeFilter = treeFilter(isOdd,listToTree(tmp_list));