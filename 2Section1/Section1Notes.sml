(* Earlier notes...

# Boolean & Comparison Operations

## Boolean Operations

(e1 : bool) andalso (e2 : bool) := if e1 then e2 else e1
(e1 : bool) orelse (e2 : bool)  := if e1 then e1 else e2
not (e : bool)                  := if e then false else true

In most other languages:
* && -> andalso
* || -> orelse
* !  -> not

andalso and orelse are keywords, not functions, because we do not evaluate every argument in these expressions

not is a function : bool -> bool because the argument is always evaluated


## Comparison Operations

For comparing int values:
=  equal; can be used for any "equality type", not real
<> not equal (typically != or =/=); can be used on any "equality type", not real
>  greater than; can also be used for real
<  less than; can also be used for real
>= geq; can also be used for real
<= leq; can also be used for real

real -> floating point, not an equality type


# Benefits of No Mutation

Alias vs copy -> doesn't matter in ML


# Piece of a Language

1. Syntax: How to write the language constructs
2. Semantics: What programs mean (evaluation rules)
3. Idioms: Typical patterns for using language features to express computation
4. Libraries: Standards provided by the language (e.g. file access, data structures, etc)
5. Tools: Language implementations provided to make coding easier (REPL, debugger, formatter, etc)

 *)
