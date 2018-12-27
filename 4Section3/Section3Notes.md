# Section 3 Notes

## Intro to First-Class Functions

### First-Class Functions
*First-class functions* can be used wherever we use values.

* Functions are values too
* Arguments, reults, parts of tuples, bound to variables, carried by datatype constructors or exceptions, ...

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
* Higher-order functions are often so "generic" and "reusable" that they have polymorphic types, i.e. tyoes with type variables
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


## Map and Filter





























