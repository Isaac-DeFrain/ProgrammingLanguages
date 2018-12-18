(* Additional Problems *)

(* pass_or_fail : {id='a, grade=int option} -> pass_fail *)
type student_id = int;
type grade = int; (* must be 0 to 100 in range *)
type final_grade = {id : student_id, grade : grade option};
datatype pass_fail = pass | fail;

fun pass_or_fail {grade=x, id=y} =
    if x = NONE
    then fail
    else
	if valOf x < 75
	then fail
	else pass;


(* tree_height = fn : 'a tree -> int *)
datatype 'a tree = leaf 
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me;

fun tree_height tr =
    case tr of
	leaf => 0
      | node{value=v,left=l,right=r} => 1 + Int.max(tree_height l, tree_height r); 

val test1 = tree_height(leaf) = 0;
val test2 = tree_height(node{value="a",left=leaf,right=leaf}) = 1;
val test3 = tree_height(node{value="a",left=node{value="a",left=leaf,right=leaf},right=leaf}) = 2;


fun nondec xs =
    case xs of
	x::(y::rest) => x <= y andalso nondec (y::rest)
      | _ => true;

val test1 = nondec [3,3,4] = true;
val test2 = nondec [1] = true;
val test3 = nondec [] = true;
