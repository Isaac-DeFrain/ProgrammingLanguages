# Section 3 Notes

## Intro to First-Class Functions

### First-Class Functions
*First-class functions* can be used wherever we use values.

* Functions are values too
* Arguments, results, parts of tuples, bound to variables, carried by datatype constructors or exceptions, ...

E.g. the first-class use of `double` and `incr` in `a_tuple`
```
	fun double x = 2 * x
	fun incr   x = x + 1
	val a_tuple  = (double, incr, double(incr 7))
```

Thus we have `double = fn : int -> int`, `incr = fn : int -> int`, and `a_tuple = (fn,fn,16) : (int -> int) * (int -> int) * int`.

The most common use of first-class functions is as an argument/result of another function.

* The function that takes/returns a function is called *higher-order*
* This yields a powerful way to "factor out" common functionality


### Function Closures
A *function closure* is a function that can use bindings from outside the function definition (in scope where function is defined)

* Makes first-class functions much more powerful
* Use things in the environment, not just arguments and local bindings


## Functions as Arguments
We can pass one function as an argument to another function. E.g.
```
	fun f (g,...) = ... g(...) ...
	fun h1   ...  = ...
	fun h2   ...  = ...
	... f(h1,... ... f(h2,...) ...
```

Makes `f` more parameterizable because different callers of `f` can pass in different functions. This is an elegant strategy for factoring out code.

* Replace `n` similar functions with calls to one function with `n` different (short) functions passed in as arguments

Three similar simple first-order functions
```
	fun incr_n_times_lame   (n,x) =
	    if n=0
	    then x
	    else 1 + incr_n_times_lame(n-1,x);

	fun double_n_times_lame (n,x) =
	    if n=0
	    then x
	    else 2 * double_n_times_lame(n-1,x);

	fun nth_tail_lame      (n,xs) = 
	    if n=0
	    then xs
	    else tl (nth_tail_lame(n-1,xs));
```

The common behavior can be factored out by defining a higher-order function which computes each of these functions depending upon an input. First, we must define a few simple functions.
```
	fun increment x = x + 1;

	fun double    x = 2 * x;
```

The higher-order function is:
```
	fun n_times (f,n,x) =
	    if n=0
	    then x
	    else f(n_times(f,n-1,x));
```

Then we have `incr_n_times_lame(n,x) = n_times(increment,n,x)`, `double_n_times_lame(n,x) = n_times(double,n,x)`, and `nth_tail_lame(n,xs) = n_times(tl,n,xs)`.


## Polymorphic Types and Functions as Arguments
* Higher-order functions are often so "generic" and "reusable" that they have polymorphic types, i.e. types with type variables
* Higher-order functions do not have to be polymorphic (example below)
* First-order functions can be polymorphic (we have studied some already, but an example is still given below)

Consider the polymorphic higher-order function `n_times` from the previous section:
```
	fun n_times (f,n,x) = (* ('a -> 'a) * int * 'a -> 'a *)
	    if n=0
	    then x
	    else f(n_times(f,n-1,x));
```

Regarding the type, `n` must be an `int` because it is compared to `0`, the return type of `f` must be the same as `x` since `x` can be the result (if `n=0`), and the argument type of `f` must be the same as the output type since we apply `f` to itself several times.

* `val x1 = n_times(double,4,7)` instantiates `'a` with `int`
* `val x2 = n_times(tl,2,[4,8,12,16])` instantiates `'a` with `int list`

Example: Higher-order function which is *not* polymorphic
```
	fun times_until_zero (f,x) = (* (int -> int) * int -> int *)
	    if x=0
	    then 0
	    else 1 + times_until_zero(f,f x)
```

Example: First-order polymorphic function
```
	fun len xs = (* 'a list -> int *)
	    case xs of
		  [] => 0
		| _::rest => 1 + len rest
```

## Anonymous Functions
*Anonymous functions* give a way to write functions without the use of a `fun` binding.

E.g. we could use a local helper function
```
	fun triple_n_times (n,x) = (* 3^n*x *)
	    let fun triple x = 3 * x
	    in
		n_times(triple,n,x)
	    end;
```

but `triple` is not actually needed in the entire function body. Thus, we could write
```
	fun triple_n_times (n,x) = n_times(let fun triple x = 3*x in triple end,n,x)
```

But why name a function if we only use it in one place and immediately pass it to another function? We can just use an *anonymous* function (i.e. an expression of the form `fn _ => _`):
```
	fun triple_n_times (n,x) = n_times((fn x => 3*x),n,x)
```

This is much better style. We don't even need to use a `fun` binding.

### Using Anonymous Functions
Most common use: passing arguments to higher-order functions (don't need a name).
* Cannot use anonymous functions for recursion, no name for make recursive calls
* Anonymous functions are well suited for single use


## Unnecessary Function Wrapping
Just because we can use anonymous functions, doesn't mean they are always appropriate. E.g. there is no need to call an anonymous function `fn x => tl x` because we already have a perfectly good function that computes the same value, namely `tl`.

Don't unnecessarily overuse anonymous functions.


## Map and Filter

### Map
`map` is a higher-order function which takes in a function and a list and returns the list with the function applied to each element. We can define `map` by
```
	fun map (f,xs) = (* ('a -> 'b) * 'a list -> 'b list *)
	    case xs of
		  [] => []
		| x::rest => (f x)::(map(f,rest));
```

The type of `map` is very interesting; given a function `f : 'a -> 'b`, it takes in an `'a list` and returns a `'b list`.

Examples of use:
```
	val x1 = map((fn x => x+1),[0,1,2,3]);
```

instantiates both `'a` and `'b` as `int` and returns `[1,2,3,4]`.
```
	val x2 = map(hd,[[1,2],[3,4,5],[6,7,8,9]]);
```

instantiates `'a` as `int list` and `'b` as `int` and returns `[1,3,6]`.

There is a similar library function `List.map`, but it is defined using *currying* (covered later).

### Filter
`filter` is a higher-order function which takes in a boolean function and a list and returns the list of elements (in order) which return true for the boolean function. We can define `filter` by
```
	fun filter (f,xs) = (* ('a -> bool) * 'a list -> 'a list *)
	    case xs of
		  [] => []
		| x::rest => if (f x)
			     then x::(filter(f,rest))
			     else filter(f,rest);
```

Given a boolean function `f : 'a -> bool`, `filter` filters a list according to this condition.

Examples of use:
```
	fun is_even v = (v mod 2 = 0);

	fun all_even xs = filter(is_even,xs);
```

Since `is_even = fn : int -> bool`, we have `xs : int list`, and `all_even` takes in an `int` list and returns the list (in order) of even elements. E.g. `all_even [1,2,4,5,7,8] = [2,4,8]`
```
	fun all_even_snd xs = filter((fn (_,v) => is_even v),xs);
```

`all_even_snd = fn : ('a * int) list -> ('a * int) list` takes in a list of tuples and returns the list of tuples (in order) whose second coordinate is even. E.g. `all_even_snd [("a",2),("b",1),("c",3),("d",6)] = [("a",2),("d",6)]`

There is a similar library function `List.filter` (also uses currying).


## Generalizing Prior Topics
Our examples of first-class functions so far have all:
* Taken one function as an argument to another function
* Processed an int or list

But first-class functions are useful anywhere for any kind of data
* Can pass several functions as arguments
* Can put functions in data structures (tuples, lists, etc.)
* Can return functions as results
* Can write higher-order functions that traverse user-defined data structures

First-class functions are useful whenever we want to abstract over "what to compute with".

### Functions Returning Functions
The following function takes in `f = fn : int -> bool` and returns one of two `int -> int` functions
```
	fun double-or-triple f =
	    if f 7
	    then fn x => 2*x
	    else fn x => 3*x;
```

`double_or_triple = fn : (int -> bool) -> int -> int` the REPL doesn't print any unnecessary parentheses. In general,
```
	t1 -> t2 -> ... -> tN-1 -> tN  means  t1 -> ( t2 -> ( ... -> (tN-1 -> tN)))
```

i.e. the `->` operator is right-associative.

E.g. we can extract functions and values
```
	val double = double_or_triple(fn x => x-3 = 4);

	val nine = double_or_triple(fn x => x = 42) 3;
```

Indeed, `double = fn : int -> int` and `nine = 9 : int`; specifically, `double = (fn x => 2*x)`.

### Other Data Structures
We can define higher-order functions over our own datatype bindings.

E.g. let
```
	datatype exp = Const of int
		     | Neg   of exp
		     | Add   of exp * exp
		     | Mult  of exp * exp
```

We will write a higher-order function to decide if in a given `exp`, every `Const` is even, or less than `10`, or etc.
```
	fun true_of_all_const (f,e) =
	    case e of
		  Const i     => f i
		| Neg e1      => true_of_all_const(e1)
		| Add(e1,e2)  => true_of_all_const(e1)
				 andalso true_of_all_const(e2)
		| Mult(e1,e2) => true_of_all_const(e1)
				 andalso true_of_all_const(e2)
```

Given some function `f : int -> bool`, we build a boolean *predicate* `true_of_all_const = fn : (int -> bool) * exp -> bool` which traverses our data sturcture and decides if a given `e : exp` satisfies the predicate.

Now we can write a predicate to test if all constants in an expression are even
```
	fun all_const_even e = true_of_all_const((fn v => v mod 2 = 0),e);
```

or if all constants in an expression are less than `10`
```
	fun all_const_less_than_ten e = true_of_all_const((fn v => v < 10),e);
```


## Lexical Scope
To understand the power of first-class functions better, we must take a step back and study the important concept of lexical scope.

We know that function bodies can use any bindings in scope. But now that functions can be passed around, we need to ask: in scope where? 

> Where the function is defined (not where it is called)!

This semantics is called *lexical scope*. We will discuss the reasons for choosing this semantics later.

E.g. consider
```
	1 val x   = 1;		(* x |-> 1 *)
	2 fun f y = x + y;	(* x |-> 1 so f = fn y => 1 + y *)
	3 val x   = 2;		(* forget x |-> 1, now x |-> 2 *)
	4 val y   = 3;		(* y |-> 3 *)
	5 val z   = f (x + y);	(* x |-> 2, y |-> 3, f = fn (x+y) => 1+(x+y), so z |-> 1+5=6 *)
```

Then `val z = 6 : int` as opposed to `7`.

This demonstrates lexical scope even without higher-order functions.

### Function Closures
Lexical scope is the reason we need *closures*, i.e. we need a way to evaluate functions in old envirnments that aren't around anymore.

We can define the semantics of functions as follows:
* A function has *two parts*
  - The code (obviosuly)
  - The environment that current when the function was defined
* This "pair" is called a *function closure* and we can call the pair, but we cannot access its pasrts individually (unlike ML pairs)
* A call evaluates the code part in the environment part (extended with the function arguments)


## Lexical Scope and Higher-Order Functions
* A function body is evaluated in the environment that was current when the function was defined/created, extended by the function argument.
* Nothing about this rule changes when we take and return functions. However, "the environment" may involve nested let-expressions, not just the top-level sequence of bindings.
* Makes first-class functions much more powerful.

E.g. returning a function
```
				"Current env"
	val x = 1;		(* x |-> 1 *)

	fun f y =
	    let
		val x = y+1	(* local binding x |-> y+1 *)
	    in
		fn z => x+y+z	(* f = fn y => fn z => 2y+1+z *)
	    end;		(* f = fn : int -> int -> int *)

	val x = 3;		(* forget x |-> 1, now x |-> 3 *)

	val g = f 4;		(* g = fn z => 9+z *)

	val y = 5;		(* y |-> 5 *)

	val z = g 6;		(* z |-> g 6 = 9+6 = 15 *)

	val h1 = f x;		(* h1 = fn u => 7+u *)

	val h2 = f y;		(* h2 = fn u => 11+u *)

	val h3 = f z;		(* h3 = fn u => 31+u *)
```

E.g. passing a function
```
	fun f g =
	    let
		val x = 3	(* irrelevant *)
	    in
		g 2		(* f = fn g => g 2 *)
	    end;		(* f = fn : (int -> 'a) -> 'a *)

	val x = 4;		(* x |-> 4 *)

	fun h y = x + y;	(* h = fn y => 4+y *)

	val z = f h;		(* z |-> 4+2 = 6 *)

	val a = h z;		(* a |-> 4+6 = 10 *)
```

These examples demonstrate how lexical scope works with passing and returning functions.


## Why Lexical Scope




























