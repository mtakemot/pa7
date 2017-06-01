# Fer-de-lance

![A fer-de-lance](https://upload.wikimedia.org/wikipedia/commons/5/51/Bothrops_asper_-_Tortuguero1.jpg)

Fer-de-lance, aka FDL, aka **F**unctions **D**efined by **L**ambdas, is a
language with anonymous, first-class functions.

## Language

### Syntax

Fer-de-lance starts with the pairs compiler (not full tuples, just pairs) and
 _adds_ the notion of a `lambda` expression for defining anonymous
functions, and allows expressions rather than just strings in function position:

```
type program = expr

type expr =
    ...
  | ELambda of string list * expr
  | EApp of expr * expr list
```

Parentheses are required around lambda expressions in FDL:

```
expr :=
    ...
  | (lambda <ids> : <expr>)
  | (lambda: <expr>)

ids :=
  | <id> , <ids>
  | <id>
```

### Semantics

Functions should behave just as if they followed a substitution-based
semantics.  This means that when a function is constructed, the program should
store any variables that they reference that aren't part of the argument list,
for use when the function is called.  This naturally matches the semantics of
function values in languages like OCaml and Python.

## Implementation

### Memory Layout and Function Values

Functions are stored in memory with the following layout: 

```
-----------------------------------------------------------------
| code ptr | arity | var1 | var2 | ... | varn | (maybe padding) |
-----------------------------------------------------------------
```

For example, in this program:

```
let x = 10 in
let y = 12 in
let f = (lambda z: x + y + z) in
f(5)
```

The memory layout of the `lambda` would be:

```
----------------------------------------------
| <address> |   1  |  20  |  24  | <padding> |
----------------------------------------------
```

There is one argument (`z`), so `1` is stored for arity.  There are two free
variables—`x` and `y`—so the corresponding values are stored in contiguous
addresses (`20` to represent 10 and `24` to represent 12).  (If the function
stored three variables instead of two, then padding would be needed).

Function _values_ are stored in variables and registers as the address
of the first word in the function's memory, but with an additional `5`
(`101` in binary) added to the value to act as a tag.

The value layout is now:

```
0xWWWWWWW[www0] - Number
0xFFFFFFF[1111] - True
0x7FFFFFF[1111] - False
0xWWWWWWW[w001] - Pair
0xWWWWWWW[w101] - Function
```

### Computing and Storing Free Variables

An important part of saving function values is figuring out the set of
variables that need to be stored, and storing them on the heap.  Our compiler
needs to generated code to store all of the _free_ variables in a function –
all the variables that are used but not defined by an argument or let binding
inside the function.  So, for example, `x` is free and `y` is not in:

```
(lambda(y): x + y)
```

In this next expression, `z` is free, but `x` and `y` are not, because `x` is
bound by the `let` expression.

```
(lambda(y): let x = 10 in x + y + z)
```

Note that if these examples were the whole program, well-formedness would
signal an error that these variables are unbound.  However, these expressions
could appear as sub-expressions in other programs, for example:

```
let x = 10 in
let f = (lambda(y): x + y) in
f(10)
```

In this program, `x` is not unbound – it has a binding in the first branch of
the `let`.  However, relative to the `lambda` expression, it is _free_, since
there is no binding for it within the `lambda`’s arguments or body.

You will write a function `freevars` that takes an `aexpr` and returns the set
of free variables (as a list):

```
let freevars (e : expr) : (string list) = 
  ...
```

You may need to write one or more helper functions for `freevars`, that keep
track of an environment.  Then `freevars` can be used when compiling `CLambda`
to fetch the values from the surrounding environment, and store them on the
heap.  In the example of heap layout above, the `freevars` function should
return `["x", "y"]`, and that information can be used in conjunction with `env`
to perform the necessary `mov` instructions.

```
  jmp after1
temp_closure_1:
  <code for body of closure> 
after1:
  mov [ebx], temp_closure_1
  mov [ebx + 4], <arity>
  mov [ebx + 8], <var1>
  ... and so on for each variable to store
  mov eax, ebx
  add eax, 5
  add ebx, <heap offset amount>
```

### Restoring Saved Variables

The description above outlines how to _store_ the free variables of a function.
They also need to be _restored_ when the function is called, so that each time
the function is called, they can be accessed.

In this assignment we'll treat the stored variables as if they were a special
kind of _local variable_, and reallocate space for them on the stack at the
beginning of each function call.  So each function body will have an additional
part of the prelude that restores the variables onto the stack, and their uses
will be compiled just as local variables are.  This lets us re-use much of our
infrastructure of stack offsets and the environment.

The outline of work here is:

- At the top of the function, get a reference to the address at which the
  function's stored variables are in memory
- Add instructions to the prelude of each function that restore the stored
  variables onto the stack, given this address
- Assuming this stack layout, compile the function's body in an environment
  that will look up all variables, whether stored, arguments, or let-bound, in
  the correct location

The second and third points are straightforward applications of ideas we've
seen already – copying appropriate values from the heap into the stack, and
using the environment to make variable references look at the right locations
on the stack.

The first point requires a little more design work.  If we try to fill in the
body of `temp_closure_1` above, we immediately run into the issue of where we
should find the stored values in memory.  We'd like some way to, say, move the
address of the function value into `eax` so we could start copying values onto
the stack:

```
temp_closure_1:
  <usual prelude>
  mov eax, <function value?>

  mov ecx, [eax + 3]
  mov [esp - 8], ecx
  mov ecx, [eax + 7]
  mov [esp - 12], ecx
  ... and so on ...
```

But how do we get access to the function value?  The list of instructions for
`temp_closure_1` may be run for many different instantiations of the function,
so they can't all look in the same place.

To solve this, we are going to augment the _calling convention_ in Fer-de-lance
to pass along the function value when calling a function.  That is, after
we save the value of `esp` on the stack, we will store the function value
itself, and *then* store the arguments. So, for example, in a call like:

```
f(4, 5)
```

We would generate code for the caller like:

```
<store return address at [esp - 4*si]>
<store esp at [esp-4*(si + 1)]>
mov [esp - 4*(si + 2)], eax  ;; assuming eax contains the address of f's closure
<code to check that eax is tagged 101, and has arity 2, etc>
mov [esp - 4*(si + 3)], 8
mov [esp - 4*(si + 4)], 10
mov eax, [eax] ;; the address of the code pointer for the function value
jmp eax        ;; call the function
<restore old esp>
```

Now the function value is available on the stack, accessible just as an
argument (e.g. with `[ebp+8]`), so we can use that in the prelude for restoration:


```
temp_closure_1:
  <usual prelude>
  mov eax, [esp - 8]

  ;; arguments live at esp - 12 and esp - 16,
  ;; so restore environment to the next available
  ;; stack slots, i.e. esp - 20 and esp - 24
  mov ecx, [eax + 3]
  mov [esp - 20], ecx
  mov ecx, [eax + 7]
  mov [esp - 24], ecx
  ... and so on ...
```

### Toplevel functions

We are leaving in toplevel functions (i.e. functions defined with `def`). 
We will let variable names shadow function names. Thus:

```
def f(x, y):
  x - y
let f = (lambda x,y: x + y) in
f(2,1)
```

returns `3`.

### Mutable Tuples

You can mutate tuples using `setfst` and `setsnd`, i.e.

```
let t = (0,0) in
let u = setfst(t, 1) in
fst(t)
```

should return `1`. You can implement recursion with mutable tuples:

```
def callfn(p, x):
  (fst(p))(x)               

def sum(x):
  let t = (0,0) in 
  let f = (lambda x: (if x == 0: 0 else: x + callfn(t, (x - 1)))) in
  let u = setfst(t, f) in
  callfn(t,x)

sum(4)

```

### Testing

As before, we will be evaluating your tests. Write your tests in the provided
`myTests.ml`.

We're releasing 5 buggy and 1 working (as far as we know) compiler.
To see the output of all these compilers with your test cases:

1. Log into `ieng6`
2. `cd` to the root of your homework (i.e. `pa6-garter-<github username>`)
3. Run `pa7_coverage_test <program>` where
   `<program>` is the string that you want to evaluate, `<heap size>` is the
   size of the heap in words, and rest are inputs to the language. Only
   `<program>` is mandatory. If you've written your program into a file, you can
   use `pa7_coverage_test "$(cat <filename>)" ...`.
4. The corresponding `.s` and `.run` files will be created inside the
   `output{i}` folder, where `{i}` corresponds to the id of the compiler.

### Notes and TODO

The case for `compile_app` has been given to you, so the bulk of the work
is in `compile_lambda`.

- Implement `compile_lambda`, assuming that there are no free variables to
  restore. Make sure only to test with lambdas that have no free variables.
- Implement `freevars`, testing as you go.
- Modify `compile_lambda` to save and restore free variables as described in
  this writeup
