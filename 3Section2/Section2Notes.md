# Section 2 Notes

# Earlier notes...

## Polymorphic Datatypes
Lists and options are datatype bindings which are *polymorphic* type constructors because they take in a type and output a different type.
* List: `fn : 'a -> 'a list`
* Option: `fn : 'a -> 'a option`

Functions might not be polymorphic, e.g.
* `sum_int_list : int list -> int` is not polymorphic
* `append : 'a list * 'a list -> 'a list` is polymorphic

### Definition of a Polymorphic Datatype
E.g.

* Option: `datatype 'a option = NONE | SOME of 'a`
* List:   `datatype 'a mylist = Empty | Const of 'a * 'a mylist`
* Tree:

```
datatype ('a,'b) tree = Node of 'a * ('a,'b) tree * ('a,'b) tree
		      | Leaf of 'b;
```

Examples of ('a,'b) trees

```

			'a  	   	'a
	        / \	       /   \
          'b  'b        'a    'b
		      / \
		    'b  'b
```

ML does type inference to determine that `xs : int list` and `sum_list : int list -> int`

```
fun sum_list xs =
    case xs of
          [] => 0
	| x :: xs' => x + sum_list xs';
```

ML does type inference to determine that `xs,ys : 'a list` and `append : 'a list * 'a list -> 'a list`

```
fun append (xs,ys) =
    case xs of
	  [] => ys
	| x :: xs' => x :: append(xs',ys);
```

ML does type inference to determine that `sum_tree : (int, int) tree -> int`

```
fun sum_tree tr =
    case tr of
	  Leaf i => i
	| Node(i,lft,rgt) => i + sum_tree lft + sum_tree rgt;
```

ML does type inference to determine that `sum_leaves : ('a, int) tree -> int`

```
fun sum_leaves tr =
    case tr of
	  Leaf i => i
	| Node(i,lft,rgt) => sum_leaves lft + sum_leaves rgt;
```

ML does type inference to determine that `count_leaves : ('a,'b) tree -> int`

```
fun count_leaves tr =
    case tr of
	  Leaf i => 1
	| Node(i,lft,rgt) => count_leaves lft + count_leaves rgt;
```

* Even though a list can be polymorphic, one cannot mix types of elements in list.
* Functions can be polymorphic depending on how the data is used.


## Pattern Matching For Each Of Types: The Truth About Functions
`val ptrn = exp`

```
fun sum_triple triple =
    let val (x,y,z) = triple
    in
	x + y + z
    end;
```

```
fun full_name r =
    let val {first=x,middle=y,last=z} = r
    in
	x ^ " " ^ y ^ " " ^ z
    end;
```

* Function arguments can be patterns too!

`fun f ptrn = exp`

`fun sum_triple (x,y,z) = x + y + z`

`fun full_name {first=x,middle=y,last=z} = x ^ " " ^ y ^ " " ^ z`

* No need to use # for records, can just use pattern matching.
* Every function in ML takes one argument and pattern matches.

"Zero arguments" is the unit pattern `()` matching the unit pattern `()`


## Type Inference pt. 1
* Pattern matching -> type inference

E.g. this will *not* type check
```
	fun sum_triple triple = #1 triple + #2 triple + #3 triple;
```

becasue the tuple, `triple` may have more than 3 entries, but this will

```
	fun sum_triple (x,y,z) = x + y + z;
```

`fun f (x,y,z) = x + z` is a function of type `int * 'a * int -> int`

Functions like `f` are called *polymorphic* because the second argument can be of many types.

## Polymorphic and Equality Types

### Polymorphism
A function `f` of type `'a list * 'a list -> 'a list` is *more general* than a function `g` of type `string list * string list -> string list` because we can simply repalce `'a` with `string` in the type of `f` to get the type of `g`.

However, `f : 'a list * 'a list -> 'a list` is *not* more general than `g : int list * string list -> int list`.

### Equality Types
`''a` represents an *equality type*, i.e. a type which can be used with the `=` operator


## Nested Patterns
`zip3 : ('a list) * ('b list) * ('c list) -> ('a * 'b * 'c) list`

```
exception ListLengthMismatch
fun zip3 list_trip =
    case list_trip of
	  ([],[],[]) => []
	| (hd1::tl1,hd2::tl2,hd3::tl3) => (hd1,hd2,hd3)::zip3(tl1,tl2,tl3)
	| _ => raise ListLengthMismatch;
```

The anonymous pattern `_` matches any pattern!

`unzip3 : ('a * 'b * 'c) list -> ('a list) * ('b list) * ('c list)`

```
fun unzip3 trip_list =
    case trip_list of
	  [] => ([],[],[])
	| (x,y,z)::tl => let val (l1,l2,l3) = unzip3 tl
			     in
				 (x::l1,y::l2,z::l3)
			     end;
```


## More Nested Patterns
```
fun nondec xs = (* fn : int list -> bool *)
    case xs of
	  [] => true
	| x::xs' => case xs' of
		   	  [] => true
			| y::ys' => x <= y andalso nodec xs';
```

Nested case expressions are clumsy. Nested pattern matching is better!

```
fun nondec xs =
    case xs of
	  [] => true
	| x::[] => true
	| x::(y::rest) => x <= y andalso nondec (y::rest);
```

Even better:

```
fun nondec xs =
    case xs of
	x::(y::rest) => x <= y andalso nondec (y::rest)
      | _ => true;
```

`datatype sgn = P | N | Z`

```
fun multisign (x1,x2) = (* int * int -> sgn *)
    let fun sign x = if x=0 then Z else if x>0 then P else N
    in
	case (sign x1,sign x2) of
	  (Z,_) => Z
	| (_,Z) => Z
	| (P,P) => P
	| (N,N) => P
	| _ => N
    end;
```

must be careful with `_` use in pattern matching because this can type check and be wrong if a case is accidentally forgotten

* Nested patterns can lead to elegant, concise code
 - Avoid nested case expressions if nested patterns are simpler e.g. unzip3 and nondec
 - Common idiom: matching against a tuple of datatypes to compare them e.g. zip3 and multisign

* Wildcards are good style: use them instead of variables if not using the data e.g. len, multisign, nondec
 - using a variable introduces a loacl binding where a wildcard does not


## Nested Patterns Precisely
The *semantics* of pattern matching: takes a pattern `p` and a value `v` and decides (1) does `v` match with `p` and (2) if so, what variable bindings are introduced?

Since patterns can nest, the *definition is elegantly recursive*, with a separate rule for each branch. Some of the rules:
* If `p` is a variable `x`, the match succeeds and `x` is bound to `v`
* If `p` is the wildcard `_`, the match succeeds and no bindings are introduced
* If `p` is `(p1,...,pn)` and `v` is `(v1,...,vn)`, the match succeeds if and only if each `pi` matches
each `vi` and the bindings are the union of all bindings from the submatches
* If `p` is `C p1`, the match succeeds if `v` is `C v1` (i.e. same constructor) and `p1` matches `v1`. The bindings are the bindings from the submatch
* ... several other forms of patterns ...

Cannot use same variable more than once in a pattern!

E.g
1. Pattern `a::b::c::d` matches all lists with length >= 3
2. Pattern `a::b::c::[]` matches all lists with length = 3
3. Pattern `((a,b),(c,d))::e` matches all nonempty lists of pairs of pairs


## Function Patterns
```
datatype AExp = Con of int
	      | Neg of AExp
	      | Add of AExp * AExp
	      | Mul of AExp * AExp;
```

Evaluation function with case expressions

```
fun old_eval e =
    case e of
	  Con i      => i
	| Neg e1     => ~ (old_eval e1)
	| Add(e1,e2) => (old_eval e1) + (old_eval e2)
	| Mul(e1,e2) => (old_eval e1) * (old_eval e2);
```

Now with pattern matching in the function definition

```
fun eval (Con i)      = i
  | eval (Neg e1)     = ~ (eval e1)
  | eval (Add(e1,e2)) = (eval e1) + (eval e2)
  | eval (Mul(e1,e2)) = (eval e1) * (eval e2);
```

```
fun append ([],ys)     = ys
  | append (x::xs',ys) = x :: append(xs',ys);
```

E.g. The function `f` defined by

```
fun f (a::b::c) = 2 + f c
  | f [] = 0;
```

will give a compile-time warning because `f` does not have patterns to match every possible input. Thus, `val x = f [1::2::3]` will produce a run-time exception.

In general, we can write case expressions as pattern matching in functions.
```
fun f x =			fun f x =
    case x of			    	f p1 = e1
	p1 => e1			.
	.				.
	.				.
	.			      | f pn = en
      | pn => en;
```

## Exceptions
Run-time condition that should be an error.

hd is a `fn : 'a list -> 'a` defined by

```
fun hd xs =
    case xs of
	  []   => raise List.Empty
	| x::_ => x;
```

Cannot take the head of [].

```
exception MyUndesirableCondition

fun mydiv (x,y) =
    if y = 0
    then raise MyUndesirableCondition
    else x div y;
```

`exception AnotherCondition of int * int`

All exceptions have type `exn`

```
fun maxlist (xs,ex) = (* int list * exn -> int *)
    case xs of
	  []     => raise ex
	| x::[]  => x
	| x::xs' => Int.max(x,maxlist(xs',ex));

val w = maxlist ([3,4,5],MyUndesirableCondition);
```

Thus, `w` is bound to `5`.

Catching/handling exceptions:

`e1 handle ex => e2` if `e1` raises the exception `ex`, then `e2` is thrown

```
val z = maxlist([],MyUndesirableCondition)
	handle MyUndesirableCondition => 42;
```

Thus, `z` is bound to `42`.

* Exception binding
```
	exception SomeException;
	exception SomeOtherException of 'a * 'b;
```

* The `raise` primitive raises/throws the given exception
```
	raise SomeException;
	raise SomeOtherException(7,"hi");
```

* A `handle` expression handles/catches an exception (if it doesn't match, exception continues to propogate)
```
	e1 handle SomeException => e2;
	e1 handle SomeOtherException(x,y) => e2;
```

* Declaring an exception just adds a constructor to type `exn`
* Can pass values of `exn` anywhere (e.g. function arguments)
* Handle can have multiple branches with patterns of type `exn`

```
exception MyException of int
fun f n =
    if n = 0
    then raise List.Empty
    else if n = 1
    then raise (MyException 4)
    else n * n;

val x = (f 1 handle List.Empty => 42) handle MyException n => f n;
```

This code binds `x` to `16`.


## Tail Recursion
Recursive function efficiency

Recursion: not necessary to mutate data

* While a program runs, there is a *call stack* of function calls that have started but not yet returned
 - Calling a function `f` pushes an instance of `f` onto the stack.
 - When a call to `f` finishes, it is popped off from the stack.
* These *stack-frames* store info like the value of local variables and "what is left to do" in the function
* Due to recursion, multiple stack-frames may be calls to the same function


E.g. the factorial function `fun fact n = if n = 0 then 1 else n * fact(n-1)` calls itself 4 times when evaluating `fact 3`:

Building the stack from the recursive calls

```
|   fact 3    |    |fact 3: 3 * _|    |fact 3: 3 * _|    |fact 3: 3 * _|
	           |   fact 2    |    |fact 2: 2 * _|    |fact 2: 2 * _|
		            	      |   fact 1    |    |fact 1: 1 * _|
					 		 |   fact 0    |
```

Popping off the stack with successive evaluation

```
|fact 3: 3 * _ |    |fact 3: 3 * _|    |fact 3: 3 * _|    |fact 3: 3 * 2|
|fact 2: 2 * _ |    |fact 2: 2 * _|    |fact 2: 2 * 1|
|fact 1: 1 * _ |    |fact 1: 1 * 1|
|  fact 0: 1   |
```

The following `fact` variation evaluates a helper function `aux` at each step of the recursion. This is called a *tail call*.

```
fun fact n =
    let fun aux(n,acc) =
	    if n = 0
	    then acc
	    else aux(n-1,acc*n)
    in
	aux(n,1)
    end;

val x = fact 3;
```

ML replaces the contents of the stack, at each step, with the value of `aux`. Thus, no popping off of the stack is required.

```
	| fact 3 | -> |aux(3,1)| -> |aux(2,3)| -> |aux(1,6)| -> |aux(0,6)|
```


## Accumulators for Tail Recursion
*Tail-recursive function*: recursive calls are *tail calls*

E.g. not tail-recursive
```
fun sum xs =
    case xs of
	  [] => 0
	| x::xs' => x + sum xs';
```

tail-recursive

```
fun sum xs =
    let aux(xs,acc) =
	case xs of
		  [] => acc
	        | x::xs' => aux(xs',x+acc)
    in
	aux(xs,0)
    end;
```

Be wary of recursive calls to the append `@` operator.

```
fun rev xs =
    case xs of
	  [] => []
	| x::xs' => (rev xs') @ [x];
```

If `len(xs)=k`, then this has `O(k^2)` runtime.

```
fun rev xs =
    let fun aux(xs,acc) =
	case xs of
		  [] => acc
	        | x::xs' => aux(xs',x::acc)
    in
	aux(xs,[])
    end;
```

If `len(xs)=k`, then this has `O(k)` runtime.

* List-append `@` within outer recursion is inefficient.
* Cons `::` is a constant time operation


## Tail Recursion: Perspective and Definition
Cases where recursive functions cannot be evaluated in a constant amount of space.
* Most obvious examples are functions that process trees.

In these cases, the natural recursive approach is the way to go.

There is a balance between optimization and code readability.

What exactly is a tail-call?

Precise definition:
A *tail-call* is a function call in *tail position* (i.e. no work to do afterwards)
* If an expression is not in tail position, then no subexpressions are in tail position.
* In `fun f p = e`, the body `e` is in tail position.
* If `if e1 then e2 else e3` is in tail position, then `e2` and `e3` are in tail position, but `e1` is not. Similiarly for case-expressions.
* If `let b1 ... bn in e end` is in tail position, then `e` is in tail position, but no binding expressions are.
* Function-call *arguments* `e1 e2` are not in tail position.
