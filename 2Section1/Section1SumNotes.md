# Section 1 Notes

## Variable binding

An ML program is a sequence of *bindings*. Each binding is type-checked and then evaluated. The type of binding depends on a *static environment*, roughly the types of the preceding bindings in the file. How a binding is evaluated depends on a *dynamic environment*, roughly the values of the preceding bindings in the file. Just saying environment, usually means dynamic environment. Context is used synonymously for static environment.

A variable binding has syntax:
```
	val x = e;
```

* `val` is a keyword, `x` can be any variable, and `e` can be any expression. The semicolon is necessary in the REPL (read-eval-print loop) to let the interpreter know that we're done typing the binding.

* Semantics: how a binding type-checks and evaluates (mostly depends on `e`)

To type-check a variable binding, we use the "current static environment" (the types of preceeding bindings) to type-check `e` (which depends on what kind of expression it is) and produce a "new static environment" that is the current static environment except with `x : T` where `e : T`.

Analogously, to evaluate a variable binding, we use the "current dynamic environment" (the values of preceding bindings) to evaluate `e` (which depends on what kind of expression it is) and produce a "new dynamic environment" that is the current environment except with `value(x) = v` where `value(e) = v`.

A *value* is an expression that "has no more computation to do" (i.e. is a result), i.e. it cannot be simplified. E.g. `17` is a value, but `8 + 9` is not. All values are expressions, but not all expressions are values.

Definitions:
* Integer constants
	* Syntax: a sequence of digits
	* Type-checking: type int in any static environment
	* Evaluation: to itself in any dynamic environment (it is a value)

* Addition
	* Syntax: "`e1 + e2`" where `e1`, `e2` are expressions
	* Type-checking: type `int` iff `e1,e2 : int`, otherwise does not type-check
	* Evaluation: `e1 -> v1` and `e2 -> v2` in the same static environment, then produce `sum(v1,v2)`

* Variables
	* Synatx: a sequences of letters, underscores, etc.
	* Type-checking: look up the variable in the current static environment and use that type
	* Evaluation: look up the variable in the current dynamic environment and use that value

* Conditionals
	* Syntax: "if `e1` then `e2` else `e3`" where `e1`, `e2`, and `e3` are expressions
	* Type-checking: using the current static environment, a conditional type-checks iff both `e1 : bool` and `e2,e3 : T` (i.e. same type). The type of the whole expression is `T`.
	* Evaluation: under the current dynamic environment, evaluate `e1`. If `result = true`, the result of evaluating `e2` under the current environment is the overall result. Otherwise, the result of evaluating `e3` under the current environment is the overall result.

* Boolean constants
	* Synatax: `true` xor `false`
	* Type-checking: type `bool` in any static environment
	* Evaluation: to itself in any dynamic environment (it is a value)

* Less-than comparison
	* Syntax: "`e1 < e2`" where `e1`, `e2` are expressions
	* Type-checking: type `bool` iff `e1,e2 : int` in the same static environment, else does not type-check
	* Evaluation: `e1 -> v1` and `e2 -> v2` in the same dynamic environment and then produce true if `v1` is less than `v2`, else produce false

When learning new language constructs, we should ask ourselves three questions: 
1. What is the syntax?
2. What are the type-checking rules?
3. What are the evaluation rules?

## Continue notes


