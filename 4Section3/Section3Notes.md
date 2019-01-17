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

Given some function `f : int -> bool`, we build a boolean *predicate* `true_of_all_const = fn : (int -> bool) * exp -> bool` which traverses our data structure and decides if a given `e : exp` satisfies the predicate.

Now we can write a predicate to test if all constants in an expression are even
```
	fun all_const_even e = true_of_all_const((fn v => v mod 2 = 0),e);
```

or if all constants in an expression are less than `10`
```
	fun all_const_less_than_ten e = true_of_all_const((fn v => v < 10),e);
```


## Lexical Scope
To understand the power of first-class functions better, we must take a step back and study the important concept of *lexical scope*.

We know that function bodies can use any bindings in scope. But now that functions can be passed around, we need to ask: in scope where?

> Where the function is defined (not where it is called)!

This semantics is called *lexical scope*. We will discuss the reasons for choosing this semantics later.

E.g. consider
```
	1 val x   = 1;		(* x |-> 1 *)
	2 fun f y = x + y;	(* x |-> 1 so f = fn y => 1 + y *)
	3 val x   = 2;		(* x |-> 2 now shadows x |-> 1 *)
	4 val y   = 3;		(* y |-> 3 *)
	5 val z   = f (x + y);	(* x |-> 2, y |-> 3, f = fn (x+y) => 1+(x+y), so z |-> 1+5=6 *)
```

Then `val z = 6 : int` as opposed to `7` (what we would get if we used the binding `x |-> 2` in our call of `f`).

This demonstrates lexical scope even without higher-order functions.

### Function Closures
Lexical scope is the reason we need *closures*, i.e. we need a way to evaluate functions in old environments that aren't around anymore.

We can define the semantics of functions as follows:
* A function has *two parts*
  - The code (obviously)
  - The environment that was current when the function was defined
* This "pair" is called a *function closure* and we can call the pair, but we cannot access its pasts individually (unlike ML pairs)
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
* Lexical scope: use environment where function is defined
* Dynamic scope: use environment where function is called

Decades ago, these were both considered reasonable, but now lexical scope is much more widely used.

Three technical reasons for using lexical scope:
1. Function meaning does not depend on variable names used
  - we can change the body of a function to use different variables

E.g. the functions `f1` and `f2` are equivalent under lexical scope
```
	fun f1 y =					fun f2 y =
	    let val x = y + 1			    let val q = y + 1
	    in					    in
		fn z => x + y + z			fn z => q + y + z
	    end;				    end;

	val x = 17;

	val a1 = (f1 7) 4;

	val a2 = (f2 7) 4;
```

Under lexical scope, we have `val a1 = 19 : int` and `val a2 = 19 : int`. However, under dynamic scope, we have `val a1 = 28 : int` and `q` is an unbound variable so `a2` is undefined.

  - we can remove unused variables

E.g. consider
```
	fun f g = let val x = 3 in g 2 end;

	fun h x = x + 1;

	val a = f h;
```

Under lexical scope, `val a = 3 : int` and under dynamic scope, `val a = 4 : int`.

2. Functions can be type-checked and reasoned about where they are defined.

E.g. consider
```
	val x = 1;

	fun f y = let val x = y + 1 in fn z => x + y + z end;

	val x = "hi";

	val g = f 7;

	val z = g 4;
```

Under lexical scope, `val z = 19 : int` and `val g = fn z => 15 + z : int -> int`. Whereas under dynamic scope, when we call `g` we try to add a string to an unbound variable.

3. Closures can easily store the data they need.

E.g. consider
```
	fun greaterThan x = fn y => y > x; (* int -> int -> bool *)

	fun noNegatives xs = filter(greaterThan ~1, xs); (* closure *)

	fun allGreater (xs,n) = filter(fn x => x > n, xs);
```

* Lexical scope for variables is the right default and is common across languages.
* Dynamic scope can be convenient for specific applications.
  - Some languages (e.g. Racket) have special ways to do it
* Loosely, exception handling acts like dynamic scope:
  - `raise e` transfers control; to the current innermost handler
  - Does not have to be syntactically inside a `handle` expression (and usually is not)


## Closures and Recomputations
Things we know about evaluation:
* A function body is *not* evaluated until the function is called.
* A function body is evaluated *every time* the function is called.
* A variable binding evaluates its expression *when the binding is evaluated*, not every time the variable is used.

With closures, this means we can avoid repeating computations that *do not depend on function arguments*.

E.g. to emphasize the semantics of functions (closures), consider
```
	fun allShorterThan1 (xs,s) =
	    filter(fn x => String.size x < String.size s, xs);
	(* recomputes String.size s for every element of xs *)

	fun allShorterThan2 (xs,s) =
	    let val size = String.size s
	    in
		filter(fn x => String.size x < size, xs)
	    end;
	(* only computes String.size s once *)
```

A caller can't tell which function is used (i.e. they do the same thing) except we will get better performance with `allShorterThan2`.

To demonstrate, we add some `print "!";` commands:
```
	fun allShorterThan1 (xs,s) =
	    filter(fn x => String.size x < (print "!"; String.size s), xs);

	fun allShorterThan2 (xs,s) =
	    let val size = (print "!"; String.size s)
	    in
		filter(fn x => String.size x < size, xs)
	    end;

	val _ = print "\nwithAllShorterThan1: ";

	val x1 = allShorterThan1(["1","333","22","4444"],"xxx");

	val _ = print "\nwithAllShorterThan2: ";

	val x2 = allShorterThan2(["1","333","22","4444"],"xxx");

	val _ = print "\n";
```

Now we will get `withAllShorterThan1: !!!!` and `withAllShorterThan2: !` indicating that we have computed `String.size "xxx"` four times in evaluating `x1` and only once in evaluating `x2`.


## Fold and More Closures
`fold` is another famous iterator over recursive data structures which accumulates an answer by repeatedly applying a function to the answer so far.
* `fold(f,acc,[x1,x2,x3,x4])` computes `f(f(f(f(acc,x1),x2),x3),x4)`

We define the `fold` function by the code
```
	fun fold (f,acc,xs) =
	    case xs of
		  [] => acc
		| x::rest => fold(f,f(acc,x),rest)
```

This version "folds left". Also `val fold = fn : ('a * 'b -> 'a) * 'a * 'b list -> 'a list`.

* These "iterator-like" functions are not built into the language, just a programming pattern.

* This pattern separates recursive traversal from data processing
  * Can reuse same traversal for different data processing
  * Can reuse same data processing for different data structures
  * In both cases, using common vocabulary concisely communicates intent

E.g. several uses of `fold`
```
	(* function: sum list *)
	fun f1 xs = fold((fn (x,y) => x + y), 0, xs);
```

```
	(* predicate: all elements non-negative? *)
	fun f2 xs = fold((fn (x,y) => x andalso y >= 0), true, xs);
```

```
	(* function: number of elements between lo and hi, inclusive *)
	fun f3 (lo,hi,xs) =
	    fold((fn (x,y) => x + (if lo <= y andalso y <= hi then 1 else 0)), 0, xs);
```

```
	(* predicate: all elements have String.size < String.size s? *)
	fun f4 (xs,s) =
	    let val size = String.size s
	    in
	    	fold((fn (x,y) => x andalso String.size y < size), true, xs)
	    end;
```

```
	(* predicate: all elements satisfy the predicate g? *)
	fun f5 (g,xs) = fold((fn (x,y) => x andalso g y), true, xs);
```

```
	fun f4again (xs,s) =
	    let val size = String.size s
	    in
		f5((fn x => String.size x < size),xs)
	    end;
```

* Functions like `map`, `filter`, and `fold` are much more powerful thanks to closures and lexical scope
* Functions passed in can use any "private" data in its environment
* Iterator "doesn't even know what data is there" or type


## Closure Idiom: Combining Functions
We have seen the rule for lexical scope and function closures and seek to answer the question of what they are good for in these next few sections.

A partial but wide-ranging list of applications:
* pass functions with private data to iterators: Done
* Combine functions (e.g. composition)
* Currying (mutli-argument functions and partial application)
* Callbacks (e.g. in reactive programming)
* Implementing an ADT with a record of functions

Function composition can be defined by
```
	fun compose (f, g) = fn x => f (g x); (* ('a -> 'b) * ('c -> 'a) -> 'c -> 'a *)
```

another way to write `compose(f,g)` is like the mathematical notation for function composition `f o g` (a lowercase `o`).

E.g. the square root of the absolute value of an integer is a composition
```
	fun sqrt_of_abs i = Math.sqrt (Real.fromInt (abs i))

	val sqrt_of_abs = Math.sqrt o Real.fromInt o abs
```

Since function composition is read "right-to-left" there is a convenient tool for making it left-to-right typically written `|>`. However, when typing the character `|` into SML mode for Emacs, it thinks it is a different language feature so we define our own *infix* notation `!>`. We use the `infix` keyword to designate the symbols as an infix operator, then define it with a function.
```
	infix !>

	fun x !> f = f x;
```

All `x !> f` does is apply the second argument to the first, effectively composing functions left-to-right. Now we can rewrite `sqrt_of_abs`
```
	fun sqrt_of_abs i = i !> abs !> Real.fromInt !> Math.sqrt
```

Sometimes this is called a *pipeline*.

Now we will define some "backup" functions. The idea is that if everything goes according to plan, then the computation runs uninterrupted, but if we don't get what we want, then change the computation. E.g.
```
	fun backup1 (f, g) = fn x => case f x of
					  NONE => g x
					| SOME y => y;
```

Here, we have `val f = fn : 'a -> 'b option`, `val g = fn : 'a -> 'b`, and `val backup1 = fn : ('a -> 'b option) * ('a -> 'b) -> 'a -> 'b`.

This is like function composition except we either return the output of `f` with the option stripped off or we return the output of `g`.

We can do a similar thing with exceptions
```
	fun backup2 (f, g) = fn x => f x handle _ => g x;
```

Now we apply `f` and return its result unless it throws an exception in which case we return the output of `g`. Here, we have `val backup2 = fn : ('a -> 'b) * ('a -> 'b) -> 'a -> 'b`.


## Closure Idiom: Currying
Recall that every function in ML actually only takes one argument which is pattern-matched against.
* Previously encoded `n` arguments as one `n`-tuple
* Another way: take one argument and return a function that takes another argument and ...

This technique is called *currying* after the logician Haskell Curry (after whom the programming language Haskell is also named).


The old way to get the effect of multiple arguments:
```
	fun sorted3_tuple (x,y,z) = z >= y andalso y >= x;

	val t1 = sorted3_tuple (7,9,11);
```

We have `val sorted3_tuple = fn : int * int * int -> bool` and `val t1 = true : bool`.

The new way: currying!
```
	val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x;

	val t2 = ((sorted3 7) 9) 11;
```

Then we have `val sorted3 = fn : int -> int -> int -> bool` and `val t2 = true : bool`. Recall that `fn : int -> int -> int -> bool` is equivalent to `fn : int -> (int -> (int -> bool))`, i.e. `->` composition is *right-associative*.

Calling `sorted3 7` returns a closure with:
* code: `fn y => fn z => z >= y andalso y >= 7`
* environment: `x |-> 7`

Calling that closure with `9` returns a closure with:
* code: `fn z => z >= 9 andalso 9 >= 7` or simply `fn z => z >= 9`
* environment: `x |-> 7, y |-> 9`

Calling that closure with `11` returns true.

### Syntactic Sugar
* In general, `e1 e2 e3 ...` means `(...(e1 e2) e3)...)`, i.e. *sequential composition* is *left-associative*
* Different than tupling; caller and callee must use same technique, i.e. we can't pass tuples to `sorted3` or "curried" arguments to `sorted3_tuple`
* In general, `fun f p1 p2 p3 ... = e` means `fun f p1 = fn p2 => fn p3 => ... => e`

Instead of `val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x` or `fun sorted3 x = fn y => fn z => z >= y andalso y >= x`, we could simply write `fun sorted3 x y z = z >= y andalso y >= x`.

As another example, we write `fold` in curried form.
```
	fun fold f acc xs =
	    case xs of
		  [] => acc
		| x::rest => fold f f(acc,xs) rest;

	fun sum xs = fold (fn (x,y) => x + y) 0 xs; (* sum list *)
```

In the ML standard library, `foldl` has `f` take its arguments in the opposite order.


## Partial Application



## Currying Wrap up



## Mutable References



## Closure Idiom: Callbacks



### Callbacks
A common idiom: Library takes functions to apply later, when an *event* occurs, e.g.
  * When a key is pressed, mouse moves, data arrives, etc.
  * When the program enters a specific state (e.g. turns in a game)

A library may accept multiple callbacks
  * Different callbacks may need different private data with different types
  * Fortunately, a function's type does not include the type of bindings in its environment (yay closures!)

### Mutable State
Mutable states are appropriate here; we want the "callbacks register" to *change*/*update* when a function to register a callback is called.

* Example callback library

Library maintains a mutable state for "what callbacks are there" and provides a function for accepting new callbacks
  * A "real" library would also support removing callbacks, etc.
  * In example, callbacks have type `int -> unit`

(the `unit` type has no "useful" content)

The entire public library is the function
```
	val onKeyEvent : (int -> unit) -> unit
```

The side-effect of `onKeyEvent` is "I'll call you back later"; because callbacks are executed for side-effects, they may also need a mutable state.

Once a key event occurs (i.e. a key is pressed), `onKeyEvent` will take `int -> unit` callback and give the `int` corresponding to the the pressed key.

### Library Implementation
```
	val cbs : (int -> unit) list ref = ref [];

	fun onKeyEvent f = cbs := f::(!cbs);

	fun onEvent i =
	    let fun loop fs =
		    case fs of
			  [] => ()
			| f::rest => (f i; loop rest)
	    in
		loop(!cbs)
	    end;
```

### Clients
Clients can only register `int -> unit`, so if any other data is needed, it must be in closure's environment; and if a function needs to "remember" something, it needs a mutable state.

E.g.
```
	val timesPressed = ref 0;

	val _ = onKeyEvent (fn _ => timesPressed := (timesPressed + 1));

	fun printIfPressed i =
	    onKeyEvent (fn j => if i = j
				then print ("you pressed " ^ Int.toString i)
				else ());
```

`onKeyEvent` adds a logger counting the number of keys pressed to the library (list of callbacks). Then when `printIfPressed i` is called it will add the conditional function, which prints "you pressed `i`" as many times as it appears in the library, to the library.


## Standard ML Documentation
Two kinds of things go into a standard library:
* Things we cannot implement on our own, e.g. opening a file, setting a timer, connecting to the network, etc.
* Very common things, e.g. `List.map`, string concatenation, etc.

[The Standard ML Basis Library](http://www.standardml.org/Basis/manpages.html) is organized into *structures* which have *signatures* using ML's module system. We'll define our own structures in the next section.

To use a binding: `StructureName.FunctionName`, e.g. `List.map`, `String.isSubstring`, etc.

The REPL can display the signatures for each structure (without special support for printing documentation like in many languages).
* Type in a function's name to see it's type
* Can also guess a function name or print the whole structure

To print all the function bindings in a structure, say, in the `List` structure:
```
	structure x = List;  (* returns structure x : LIST *)

	signature x = LIST;
```

## Optional
### Abstract Data Types with Closures
*Abstract data types* are some times abbreviated by ADT.

Closures can implement ADTs.
* Can put multiple functions into a record
* The functions can share the same private data
* Private data can be mutable or immutable (here we choose immutable)
* Feels a lot like objects, emphasizing that OOP and functional programming have some depp similarities

We will implement immutable sets of integers with the operations *insert*, *member*, and *size*. The sets will be represented by a record.

The code is advanced/clever/tricky, but it has no new features
* Combines lexical scope, datatypes, records, closures, etc.
* Client use is straightforward

We want to implement something like
```
	type set = {insert : int -> set, member : int -> bool, size : unit -> int};
```

However, ML does not allow for type synonyms to be recursively defined. Instead, we will use a datatype binding
```
	datatype set = S of {insert : int -> set, member : int -> bool, size : unit -> int};
```

So `S` is the only constructor for this datatype.

The only public value is `empty_set : set` (defined after example client).

### An Example Client (the easy stuff)
```
	fun use_sets () = (* val use_sets = fn : unit -> int *)
	    let val S s1 = empty_set
		val S s2 = (#insert s1) 34
		val S s3 = (#insert s2) 34
		val S s4 = (#insert s3) 19
	    in
		if (#member s4) 42
		then 99
		else if (#member s4) 19
		then 17 + (#size s3) ()
		else 0
	    end;
```

### Implementation of Sets (the fancy stuff)
```
	val empty_set =
	    let fun make_set xs = (* xs is a "private field" in result *)
		    let           (* contains a "private method" in result *)
		        fun contains i = List.exists (fn j => i = j) xs
		    in
		        S { insert = fn i => if contains i
					     then make_set xs
					     else make_set (i::xs),
			    member = contains,
			    size = fn () => length xs
		          }
		    end
	    in
		make_set []
	    end;
```

We seed the datatype `set` with the `empty_set` which can now be used to generate other `set`s by invoking the `insert` function as the above client has done.


## Next Section
