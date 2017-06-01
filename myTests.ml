open Runner

(** Fill in myTestList with your own tests.

    There are two ways to do so:

    1) To test that a program produces the desired result:

       t   <name of test> <code> <expected result>
       t_i <name of test> <code> <expected result> [<input1>; <input2>; ...]

    e.g.

      t   "Add"  "1+1"        "2"
      t_i "Add2" "input(0)+1" "2" ["1"]

    2) To test that a program throws an expected error:

      terr   "Error"  "1+true"     "expected a number"
      terr_i "Error2" "1+input(0)" "expected a number" ["true"]

    3) To write long-form tests in a separate file, place your code in

       input/<filename>.egg

       and use "(src <filename>)" in place of <code>

**)

let myTestList =
  [
  ]
;;
