fun reverse[]=[]
   |reverse(a::L)=(reverse L)@[a];
   
fun reverse'(L:int list):int list =
    let
      fun helper(L : int list, A : int list): int list =
        case L of
        [] => A
        |x::R => helper(R, x::A)
    in
      helper(L, [])
    end


fun interleave(A:int list,B:int list):int list =
    case (A,B) of
    ([],_) => B
    |(_,[])=> A
    |(x::X,y::Y)=>x::y::interleave(X,Y)；

datatype tree = Empty | Node of tree * int * tree;
	
fun listToTree([]:int list):tree = Empty
  | listToTree(x::X) = 
	let
      val k = (length X)div 2
    in
      Node(listToTree(List.take(X,k)),x,listToTree(List.drop(X,k)))
	end
	
fun revT(Empty:tree):tree=Empty
  | revT(Node(left,x,right)) = Node(revT(right),x,revT(left))；
	
fun binarySearch(Empty:tree,i:int): bool = false 
  | binarySearch(Node(left,x,right),i) =
	case Int.compare(x,i) of
	GREATER => binarySearch(left,i)      
    | EQUAL => true 
    | LESS => binarySearch(right,i)；  

  
	
