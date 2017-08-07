lispboot
========

lispboot is a general boot code used in [ocalisp](https://github.com/yubrot/ocalisp), [golisp](https://github.com/yubrot/golisp) and [scalisp](https://github.com/yubrot/scalisp).

## Required syntax

* `(def sym x)` - Evaluates `x`, and then binds a new variable named `sym` to the result of `x` on the current environment. Produces `()`.
* `(set! sym x)` - Evaluates `x`, and then overwrites a variable named `sym` on the current environment. Produces `()`.
* `(begin ...)` - Evaluates arguments sequentially and produces the result of the last argument.
* `(if cond a b)` - Evaluates `cond`, and then evaluates `a` if the result of `cond` is not `#f`, or otherwise evaluates `b`. Produces the result of `a` or `b`.
* `(fun params ...)` - Creates a function and produces it.
* `(macro params ...)` - Creates a macro and produces it.
* `(builtin sym)` - Captures a builtin function named `sym` and produces it.
* `(quote x)` - Produces a value represented by `x`.

## Required builtin functions

```
(cons a b)

(exit exitcode)
(error msg)

(gensym)

(car cons)
(cdr cons)

(apply f args)

(num? x)
(sym? x)
(str? x)
(cons? x)
(nil? x)
(bool? x)
(proc? x)
(meta? x)
(port? x)
(vec? x)

(+ nums...)
(- num nums...)
(* nums...)
(/ num nums...)
(% num nums...)

(= xs...)
(< nums-or-strs...)
(> nums-or-strs...)
(<= nums-or-strs...)
(>= nums-or-strs...)

(call/cc fun)

(str bytes...)
(str-ref str n)
(str-bytesize str)
(str-concat strs...)
(substr str n bytesize)
(sym->str sym)
(num->str num)
(str->num str)

(vec items...)
(vec-make length init)
(vec-ref vec n)
(vec-length vec)
(vec-set! vec n item)
(vec-copy! dest dest-start src src-start length)

(open filepath mode) ; mode = "r" | "w"
(close port)

(stdin)
(stdout)
(stderr)

(read-byte port)
(read-str bytesize port)
(read-line port)
; Returns 'eof instead of data read on EOF

(write-byte byte port)
(write-str str port)
(write-line str port)

(args)

(eval s)
(macroexpand s)
(macroexpand-1 s)
```

