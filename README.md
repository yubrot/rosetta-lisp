# Rosetta Lisp

Rosetta Lisp is a set of Lisp-1 implementations that share the VM instruction set, built-in functions, and the bootstrapping code.

- [Introduction article (Japanese)](https://zenn.dev/yubrot/articles/0ea5405ea53de5)

## Implementations

Currently there are 6 implementations available. Each of them is implemented in different languages.

* [ocalisp](https://github.com/yubrot/ocalisp) in OCaml, reference implementation
* [scalisp](https://github.com/yubrot/scalisp) in Scala
* [golisp](https://github.com/yubrot/golisp) in Go
* [fslisp](https://github.com/yubrot/fslisp) in F#
* [idrlisp](https://github.com/yubrot/idrlisp) in Idris
* [wonderlisp](https://github.com/yubrot/wonderlisp) in Rosetta Lisp itself

## VM Design

The design of the Rosetta Lisp implementation is heavily inspired by [SECD machine](https://en.wikipedia.org/wiki/SECD_machine). Every S-expression is macro-expanded and then compiled into a code, a sequence of instructions. Rosetta Lisp implementations share an extremely simple, small instruction set that targeting an SECD-like virtual machine.

### VM Instruction Set

Rosetta Lisp VM consists of four states:

- `[S]tack` - a list of values
- `[E]nvironment` - an abstract representation of a collection of key-value pairs
- `[C]ode` - a list of instructions
- `[D]ump` - a list of pairs of Environment and Code

#### ldc value

Push a `value` on top of `S`.

```lisp
(define (ldc value)
  (lambda (S E C D k)
    (k (cons value S) E C D)))
```

#### ldv name

Find a value named `name` from `E` and push it on top of `S`.

```lisp
(define (ldv name)
  (lambda (S E C D k)
    (k (cons (lookup-env name E) S) E C D)))
```

#### ldf pattern code

Make a function capturing `E` and push it on top of `S`.

```lisp
(define (ldf pattern code)
  (lambda (S E C D k)
    (k (cons (make-function-closure pattern code E) S) E C D)))
```

#### ldm pattern code

Make a macro capturing `E` and push it on top of `S`.

```lisp
(define (ldm pattern code)
  (lambda (S E C D k)
    (k (cons (make-macro-closure pattern code E) S) E C D)))
```

#### ldb name

Find a built-in function named `name` and push it on top of `S`.

```lisp
(define (ldb name)
  (lambda (S E C D k)
    (k (cons (find-builtin-function name) S) E C D)))
```

#### sel then-code else-code

Pop a value from `S`, set `C` to `then-code` if the value is `#f`, set `C` to `else-code` otherwise. Set `E` to a new environment derived from `E`. Push the previous `E` and `C` on top of `D`.

```lisp
(define (sel then-code else-code)
  (lambda ((s . S) E C D k)
    (k S
       (new-env E)
       (if s then-code else-code)
       (cons (cons E C) D))))
```

#### leave

Pop a pair of Environment and Code from `D` and set `E` and `C` to it.

```lisp
(define (leave)
  (lambda (S E C ((e . c) . D) k)
    (k S e c D)))
```

#### app argc

Pop `argc` values as function arguments from `S`, pop a function from `S`, apply the function with the arguments.

```lisp
(define (app argc)
  (lambda ((argN argN-1 ... arg1 f . S) E C D k)
    (apply-function f (list arg1 .. argN) S E C D k)))
```

`apply-function` is defined for built-in functions and function closures obtained by `ldf`.
`apply-function` on function closures is defined as follows:

```lisp
(define (apply-function (FUNCTION-CLOSURE pattern code env)
                        args
                        S E C D k)
  (k S
     (bind-args-with-pattern args pattern (new-env env))
     code
     (cons (cons E C) D)))
```

#### pop

Pop a value from `S`.

```lisp
(define (pop)
  (lambda ((s . S) E C D k)
    (k S E C D)))
```

#### def name

Pop a value, create a binding from `name` to the value on `E`.

```lisp
(define (def name)
  (lambda ((s . S) E C D k)
    (k S (env-define name s E) C D)))
```

#### set name

Pop a value, update a binding from `name` to the value on `E`.

```lisp
(define (set name)
  (lambda ((s . S) E C D k)
    (k S (env-set name s E) C D)))
```

### Compilation to VM instructions

All literals and quoted expressions are compiled into `ldc`:
```
; compiling 123
[0 entry]
  ldc 123
```

All unquoted symbols are compiled into `ldv`:
```
; compiling foo
[0 entry]
  ldv foo
```

Lists are compiled into a sequence of element evaluations and an `app`.
```
; compiling (compare a b)
[0 entry]
  ldv compare
  ldv a
  ldv b
  app 2

; compiling (+ foo (* bar baz) 111)
[0 entry]
  ldv +
  ldv foo
  ldv *
  ldv bar
  ldv baz
  app 2
  ldc 111
  app 3
```

All other instructions are produced by __Syntax__. Applications of Syntax are not compiled into an usual `app`.

`(builtin cons)` produces a `ldb`.
```
[0 entry]
  ldb hello
```

`(if a b c)` produces a `sel` and two branch codes terminated by a `leave`.
```
[0 entry]
  ldv a
  sel [1 then] [2 else]
[1 then]
  ldv b
  leave
[2 else]
  ldv c
  leave
```

`(begin a b c)` produces a sequence of evaluations and `pop`s.
```
[0 entry]
  ldv a
  pop
  ldv b
  pop
  ldv c
```

`(fun (x y) y)` produces a `ldf`, `(macro (x y) x)` produces a `ldm`. Body of functions are terminated by a `leave` but macros are not.
```
[0 entry]
  ldf [1 fun (x y)]
[1 fun (x y)]
  ldv y
  leave
```
```
[0 entry]
  ldm [1 macro (x y)]
[1 macro (x y)]
  ldv x
```

`(def x 123)` produces a `def`, `(set! x 123)` produces a `set`. Both of them also produce a `ldc ()` to adjust stack size.
```
[0 entry]
  ldc 123
  def x
  ldc ()
```
```
[0 entry]
  ldc 123
  set x
  ldc ()
```

`(quote x)` is also one of Syntax that produces a `ldc`.

## Built-in functions

All other functionalities are injected as built-in functions. Every implementation provides the same built-in function set. By doing this, Every implementation can use [the same bootstrapping code](./boot.lisp) to get frequently used functions and macros.

### Built-in functions required by the bootstrapping code

To reduce the size of required built-in functions, there are a lot of functions and macros that are defined in the bootstrapping code instead of in host languages. It's not optimal in terms of performance, but it makes easy to add another Rosetta Lisp implementation.

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
(never fun args...)

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
(flush port)

(args)

(eval s)
(macroexpand s)
(macroexpand-1 s)
```

## Tests

There are [test suites](./testsuites) for the functionalities of the parser, the compiler, and the VM.
Rosetta Lisp's bootstrapping code includes unit tests for each built-in function, as comments.

```lisp
(def cons (builtin cons))
;! > (cons 1)
;! fail
;! > (cons 1 2)
;! (1 . 2)
;! > (cons 1 2 3)
;! fail
```

Since it's troublesome to parse these comments, there is [a unified, easy-to-parse test file](./test) available. This test file is generated by [./gentest](./gentest).
