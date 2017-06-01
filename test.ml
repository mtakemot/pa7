open Compile
open Runner
open Printf
open OUnit2
open MyTests

let forty_one = "sub1(42)"
let forty = "sub1(sub1(42))"
let def_x = "let x = 5 in x"
let def_x2 = "let x = 5 in sub1(x)"
let def_x3 = "let x = 5 in let x = 67 in sub1(x)"
let addnums = "5 + 10"
let single_nest = "(5 + 2) - 1"
let negatives = "(0 - 5) + (0 - 10)"
let negatives2 = "(0 - 1) + (0 - 1)"
let nested_add = "(5 + (10 + 20))"
let let_nested = "let x = (5 + (10 + 20)) in x * x"

let if_simple_true = "if true: 1 else: 2"
let if_simple_false = "if false: 1 else: 2"
let nested_if = "if if false: 1 else: true: 11 else: 2"

let greater_of_equal = "4 > 4"
let less_of_equal = "4 < 4"
let greater = "4 > 3"
let less = "3 < 4"
let not_greater = "2 > 3"
let not_less = "3 < 2"

let equal = "(0 - 2) == (0 - 2)"
let not_equal = "(0 - 1) == (0 - 2)"

let add_true_left   = "true + 4"
let add_true_right  = "1 + true"
let add_false_left  = "false + 4"
let add_false_right = "1 + false"

let overflow = "1073741823 + 2"
let underflow = "(0 - 1073741823) - 2"

let print = "print(add1(5))"

let err_if_simple_true = "if 0: 1 else: 2"
let err_if_simple_false = "if 54: 1 else: 2"
let err_nested_if = "if if 54: 1 else: 0: 11 else: 2"

let lottalet = "
let
  a = 1,
  b = print(a + 1),
  c = print(b + 1),
  d = print(c + 1),
  e = print(d + 1),
  f = print(e + 1),
  g = print(f + 1) in
g"

let lottalet2 ="
let
  a = 1,
  b = print(a + 1),
  c = print(b + 1),
  d = print(a + 1),
  e = print(b + 1),
  f = print(c + 1),
  g = print(a + 1) in
g"

let ibt = "isbool(true)"
let ibf = "isbool(false)"
let intr = "isnum(true)"
let infalse = "isnum(false)"
let ibz = "isbool(0)"
let ib1 = "isbool(1)"
let ibn1 = "isbool(-1)"
let inz = "isnum(0)"
let in1 = "isnum(1)"
let inn1 = "isnum(-1)"

let call1="
def f(x, y):
  x - y
f(1, 2)
"

let call2 = "
def g(y):
  y
def f(x, y):
  g(x)
f(1, 2)
"

let call3 = "
def f(x, y):
  x
f(2, 3)
"

let call4 = "
def f(x, y):
  x
f(2, 4)
"

let fact1 = "
def fact(n):
  if n < 1: 1
  else: n * fact(n - 1)
fact(4)
"

let fact2 = "
def fact(acc, n):
  if n < 1: acc
  else: fact(acc * n, n - 1)
fact(1, 4)
"

let oddeven = "
def odd(n):
  if n == 0: false
  else: if n == 1: true
  else:
    even(n - 1)
def even(n):
  if n == 0: true
  else: if n == 1: false
  else:
    odd(n - 1)
let x = print(odd(5)) in
let y = print(even(3010)) in
odd(4)
"
            
let closure00 = "
let f = (lambda x: x + 1)
in f(9)
"            

let calls = [
  t "call1" call1 "-1";
  t "call2" call2 "1";
  t "call3" call3 "2";
  t "call4" call4 "2";

  t "fact1" fact1 "24";
  t "fact2" fact2 "24";
  t "oddeven" oddeven "true\ntrue\nfalse";
  
  t "closure00" closure00 "10";
]

let reg =
  [ t "def_x" def_x "5"
  ; t "def_x2" def_x2 "4"
  ; t "def_x3" def_x3 "66"
  ; t "addnums" addnums "15"
  ; t "single_nest" single_nest "6"
  ; t "negatives" negatives "-15"
  ; t "negatives2" negatives2 "-2"
  ; t "nested_add" nested_add "35"
  ; t "let_nested" let_nested "1225"
  ; t "if_simple_true" if_simple_true "1"
  ; t "if_simple_false" if_simple_false "2"
  ; t "nested_if" nested_if "11"

  ; t "lottalet" lottalet "2\n3\n4\n5\n6\n7\n7"
  ; t "lottalet2" lottalet2 "2\n3\n2\n3\n4\n2\n2"

  ; t "greater_of_equal" greater_of_equal "false"
  ; t "less_of_equal" less_of_equal "false"
  ; t "greater" greater "true"
  ; t "less" less "true"
  ; t "not_greater" not_greater "false"
  ; t "not_less" not_less "false"

  ; t "equal" equal "true"
  ; t "not_equal" not_equal "false"

  ; t "print" print "6\n6"

  ; t "ibt" ibt "true"
  ; t "ibf" ibf "true"
  ; t "intrue" intr "false"
  ; t "infalse" infalse "false"
  ; t "ibz" ibz "false"
  ; t "ib1" ib1 "false"
  ; t "ibn1" ibn1 "false"
  ; t "inz" inz "true"
  ; t "in1" in1 "true"
  ; t "inn1" inn1 "true"

  ; terr "add_true_left"   add_true_left   "expected a number"
  ; terr "add_true_right"  add_true_right  "expected a number"
  ; terr "add_false_left"  add_false_left  "expected a number"
  ; terr "add_false_right" add_false_right "expected a number"

  ; terr "err_if_simple_true" err_if_simple_true "expected a boolean"
  ; terr "err_if_simple_false" err_if_simple_false "expected a boolean"
  ; terr "err_nested_if" err_nested_if "expected a boolean"

  ; terr "overflow" overflow "overflow"
  ; terr "underflow" underflow "overflow"
  ]


let errs =
  [ terr "prim1_1" "add1(true)" "expected a number"
  ; terr "prim1_2" "add1((1,2))" "expected a number"
  ; terr "prim1_3" "sub1(true)" "expected a number"
  ; terr "prim1_4" "sub1((1,2))" "expected a number"
  ; terr "prim1_5" "input(true)" "expected a number"
  ; terr "prim1_6" "input((1,2))" "expected a number"

  ; terr "unbound" "x" "variable identifier x"
  ; terr "unbound2" "let x = 10 in y" "variable identifier y"
  ; terr "multiple1" "let x = 1, x = 2 in x + 1" "Multiple bindings for variable identifier x"

  ; terr "duplicate" "def f(x, x): x\n9" "Duplicate parameter"
  ; terr "duplicate2" "def g(x): x\ndef f(x, x): x\n9" "Duplicate parameter"

  ; terr "duplicate_let" "let x = 10, x = 5 in x" "Multiple bindings for variable identifier x"
  ; terr "duplicate_let2" "let x = 10, y = 7, x = 5 in x" "Multiple bindings for variable identifier x"
  ; terr "duplicate_let3" "let x = 10, y = 7, y = 5 in x" "Multiple bindings for variable identifier y"
  ; terr "duplicate_let4" "def f(x): x + 5 let x = f(1,2) in x + 5" "Arity mismatch: f"

  ; terr "duplicate_fun" "def f(): 5\n def f(): 10\nf()" "Duplicate function"
  ; terr "duplicate_fun2" "def f(): 5\n def g(): 5\n def f(): 10\nf()" "Duplicate function"
  ]

let input_tests =
 [ t_i "input1" "input(0)" "42" ["42"]
 ; t_i "input2" "input(0) + input(1)" "85" ["42"; "43"]

 ;  terr_i "inputerr1" "input(0) + 1" "expected a number" ["true"]
 ; terr_i "inputerr2" "if input(0): 1 else: 0" "expected a boolean" ["1"]
 ]


let suite =
"suite">:::
  List.concat [ calls
              ; reg
              ; errs
              ; input_tests
              ; myTestList
              ]


let () =
  run_test_tt_main suite
;;
