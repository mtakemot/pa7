open Printf
open Expr
open Instruction
open Misc

type 'a envt = (string * 'a) list

let count = ref 0
let gen_temp base =
  count := !count + 1;
  sprintf "temp_%s_%d" base !count

let find_decl (ds : decl list) (name : string) : decl option =
  find_f (fun (DFun(fname,_,_)) -> fname = name) ds

let type_mask ty = HexConst
  (match ty with
   | TNum     -> 0x1
   | TBool    -> 0x3
   | TPair    -> 0x1
   | TClosure -> 0x7
  )

let type_tag ty = HexConst
  (match ty with
   | TNum     -> 0x0
   | TBool    -> 0x3
   | TPair    -> 0x1
   | TClosure -> 0x5
  )

let typecheck ty =
  [ IAnd (Reg EAX, type_mask ty)
  ; ICmp (Reg EAX, type_tag ty)
  ]

let const_true  = HexConst(0xffffffff)
let const_false = HexConst(0x7fffffff)

let throw_err code =
  [ IPush (Sized(DWORD_PTR, Const code))
  ; ICall "error"
  ]

let error_overflow  = "overflow_check"
let error_non_int   = "error_non_int"
let error_non_bool  = "error_non_bool"
let error_non_pair = "error_non_pair"
let error_non_closure = "error_non_closure"
let error_too_small = "error_too_small"
let error_too_large = "error_too_large"
let error_arity_mismatch = "error_arity_mismatch"

let check_overflow  = IJo(error_overflow)

let check_eax_num     = typecheck TNum @ [ IJne(error_non_int) ]
let check_eax_pair    = typecheck TPair @ [ IJne error_non_pair ]
let check_eax_closure = typecheck TClosure @ [IJne error_non_closure]


let stackloc      i =   RegOffset (-4 * i, ESP)
let save_to_stack i = [ IMov (stackloc i, Reg(EAX)) ]
let restore_stack i = [ IMov (Reg(EAX), stackloc i) ]

let c_call (si : int) (f : string) : instruction list =
  [ ISub (Reg ESP, Const (4 * si))
  ; IPush (Sized (DWORD_PTR, Reg EAX))
  ; ICall f
  ; IAdd (Reg ESP, Const (4 * (si +1)))
  ]

let compare cmp =
  let jump_of_cmp l = match cmp with
    | Less -> IJl l | Equal -> IJe l | Greater -> IJg l
    | _ -> failwith "impossible"
  in
  let label_true = gen_temp "true" in
  let label_done = gen_temp "done" in
  [ jump_of_cmp label_true
  ; IMov (Reg EAX, const_false)
  ; IJmp (Label(label_done))
  ; ILabel label_true
  ; IMov (Reg EAX, const_true)
  ; ILabel label_done
  ]

let is_type ty = typecheck ty @ compare Equal

(** Fill me in and use me in compile_lambda **)
let freevars (e: expr) : string list = failwith "TBD: freevars"

let rec compile_prim1 (o : prim1) (e : expr)
                      (si : int) (ds : decl list) (env : int envt) : instruction list =
  let prelude = compile_expr e si ds env in
  let isnum   = save_to_stack si @ check_eax_num @ restore_stack si in
  let tuple_access tuple ind = 
    let tuple_inst  = compile_expr tuple si ds env @ save_to_stack si @ check_eax_pair in
    let elem_inst   = [ IMov(Reg EAX, stackloc si)
                      ; IMov(Reg EAX, RegOffset(ind*4 - 1, EAX))
                      ] in
    tuple_inst @ elem_inst
  in
  let op_is   = match o with
    | Add1    -> isnum @ [ IAdd(Reg(EAX), Const(2)) ]
    | Sub1    -> isnum @ [ IAdd(Reg(EAX), Const(-2)) ]
    | IsNum   -> is_type TNum
    | IsBool  -> is_type TBool
    | IsPair  -> is_type TPair
    | Print   -> c_call si "print"
    | Input   -> isnum @ c_call si "input"
    | Fst     -> tuple_access e 0
    | Snd     -> tuple_access e 1

  in prelude @ op_is

and compile_prim2 (o : prim2) (e1 : expr) (e2 : expr)
    (si : int) (ds : decl list) (env : int envt) : instruction list =
  let tuple_set tuple exp ind = 
    let tuple_inst  = compile_expr tuple si ds env @ save_to_stack si @ check_eax_pair in
    let exp_inst    = compile_expr exp (si+1) ds env @ save_to_stack (si+1) in
    let elem_inst   = [ IMov(Reg ECX, stackloc si)
                      ; IMov(Reg EAX, stackloc (si+1))
                      ; IMov(RegOffset(ind*4 - 1, ECX), Reg EAX)
                      ] in
    tuple_inst @ exp_inst @ elem_inst
  in
  match o with
  | Equal  -> compile_equals e1 e2 si ds env
  | SetFst -> tuple_set e1 e2 0
  | SetSnd -> tuple_set e1 e2 1
  | Plus | Minus | Times | Less | Greater 
    -> compile_prim2_helper o e1 e2 si ds env

and compile_equals (e1 : expr) (e2 : expr)
                   (si : int) (ds : decl list) (env : int envt) : instruction list =
    compile_expr e2 si ds env
  @ save_to_stack si
  @ compile_expr e1 (si+1) ds env
  @ [ ICmp (Reg EAX, stackloc si) ]
  @ compare Equal

and compile_prim2_helper (o : prim2) (e1 : expr) (e2 : expr)
                         (si : int) (ds : decl list) (env : int envt) : instruction list =
  let rhs_loc = stackloc (si+1) in
  let e1_is   = compile_expr e1 si ds env     @ save_to_stack si     @ check_eax_num in
  let e2_is   = compile_expr e2 (si+1) ds env @ save_to_stack (si+1) @ check_eax_num in
  let op_is   = match o with
    | Plus    -> [ IAdd (Reg(EAX), rhs_loc); check_overflow ]
    | Minus   -> [ ISub (Reg(EAX), rhs_loc); check_overflow ]
    | Times   -> [ ISar (Reg(EAX), Const(1))
                 ; IMul (Reg(EAX), rhs_loc)
                 ; check_overflow
                 ]
    | Less
      | Greater -> ICmp (Reg(EAX), rhs_loc) :: compare o
    | Equal     -> []
    | _ -> failwith "Unexpected op in prim2 helper"
  in     e1_is
       @ e2_is
       @ restore_stack si
       @ op_is

and compile_let (bs : (string * expr) list) (body : expr)
                (si : int) (ds : decl list) (env : int envt) : instruction list =
  match bs with
  | []         -> compile_expr body si ds env
  | (x,e)::bs' ->
       compile_expr e si ds env
     @ save_to_stack si
     @ compile_let bs' body (si+1) ds ((x,si)::env)

and compile_if (cond : expr) (e_then : expr) (e_else : expr)
               (si : int) (ds : decl list) (env : int envt) : instruction list =
  let cond_is    = compile_expr cond   si ds env in
  let then_is    = compile_expr e_then si ds env in
  let else_is    = compile_expr e_else si ds env in
  let label_then = gen_temp "then" in
  let label_else = gen_temp "else" in
  let label_end  = gen_temp "end"  in
    cond_is
  @ [ ICmp(Reg(EAX), const_true)
    ; IJe(label_then)
    ; ICmp(Reg(EAX), const_false)
    ; IJe(label_else)
    ; IJmp(Label(error_non_bool))
    ; ILabel(label_then)
    ]
  @ then_is
  @ [ IJmp(Label(label_end)); ILabel(label_else) ]
  @ else_is
  @ [ ILabel(label_end) ]
  
and compile_lambda (args : string list) (body : expr)  
                   (si : int) (ds : decl list) (env : int envt) : instruction list = 
  failwith "TBD: compile_lambda"

and compile_app (fe : expr) (args : expr list)
                (si : int)  (ds : decl list) (env : int envt) : instruction list =
  let put_args es offset env =
    let put1 i e = compile_expr e (offset + i) ds env
                @ [ IMov (Sized(DWORD_PTR,stackloc (offset + i)), Reg EAX) ]
    in flatmapi put1 es
  in
  let lbl = gen_temp "after_call" in

  let prelude = 
    (** Set up the first two stack slots with return address and ESP **)
    [ IMov(Sized(DWORD_PTR, stackloc si), Label lbl)
    ; IMov(stackloc (si+1), Reg ESP) ]
  in
  let compile_app_decl f =
    (** If "f" is a decl'd function, then follow the old convention **)
      put_args args (si+2) env
    @ [ ISub(Reg ESP, Const (si*4))
      ; IJmp (Label(f))
      ; ILabel lbl
      ; IMov(Reg ESP, stackloc 2) ]
  in
  let compile_app_closure () = 
    (** if fe is a closure, then: **)
    let closure_ptr_loc = si + 2 in
       (** get the address of the closure in EAX, store above ESP **)
       compile_expr fe (si + 2) ds env 
      @ [ IMov (stackloc closure_ptr_loc, Reg EAX) ]
       (** make sure the thing is actually a closure **)
      @ check_eax_closure
       (** clear the tag bits **)
      @ [ IAnd(stackloc closure_ptr_loc, Sized(DWORD_PTR, Const(0xFFFFFFF8))) ]
       (** check that the number of arguments matches the function arity **)
      @ [ IMov(Reg EAX, stackloc closure_ptr_loc)
        ; IMov(Reg EAX, RegOffset (4, EAX))
        ; ICmp(Reg EAX, Const(List.length args))
        ; IJne(error_arity_mismatch)
        ]
       (** now push the arguments on the stack **)
      @ put_args args (si+3) env
       (** jump to the closure's code by dereferencing the first word of the closure **)
      @ [ ISub(Reg ESP, Const (si*4))
        ; IMov(Reg EAX, stackloc 2)
        ; IJmp(RegOffset(0, EAX))
        ; ILabel lbl
        ; IMov(Reg ESP, stackloc 2)
        ]
  in match fe with
     | EId(f) when find env f <> None  ->
        (** fe is a variable expression, so it must be a closure **)
        prelude @ compile_app_closure ()
     | EId(f) when find_decl ds f <> None -> 
        (** f is not a local variable, so it must be a "def"'d function **)
        prelude @ compile_app_decl f
     | _ -> 
        (** fe is some arbitrary expression, so it must be a closure **)
        prelude @ compile_app_closure ()

and compile_pair (e1: expr) (e2: expr) (si: int) (ds : decl list) (env: int envt) : instruction list =
    compile_expr e1 si ds env
  @ [ IMov (Sized(DWORD_PTR, RegOffset(0, EBX)), Reg(EAX)) ]
  @ compile_expr e2 si ds env
  @ [ IMov (Sized(DWORD_PTR, RegOffset(4, EBX)), Reg(EAX))
    ; IMov (Reg(EAX), Reg(EBX))
    ; IAdd (Reg(EAX), Const(1))
    ; IAdd (Reg(EBX), Const(8))
    ]

and compile_expr (e : expr) (si : int) (ds : decl list) (env : int envt) : instruction list =
  match e with
  | ENumber (n ) -> [ IMov (Reg(EAX), Const((n lsl 1))) ]
  | EBool   (b ) -> [ IMov (Reg(EAX), if b then const_true else const_false) ]
  | EId     (rx) ->
     let arg = begin match find env rx with
               | Some(i) -> stackloc i
               | None    -> failwith "unbound identifier"
               end
     in [ IComment ("Look up " ^ rx); IMov (Reg(EAX), arg) ]
  | EPrim1 (op, e')          -> compile_prim1    op e'         si ds env
  | EPrim2 (op, e1, e2)      -> compile_prim2    op e1 e2      si ds env
  | ELet   (bs, body)        -> compile_let      bs body       si ds env
  | EIf    (c, ethen, eelse) -> compile_if       c ethen eelse si ds env
  | EApp   (f, args)         -> compile_app      f args        si ds env
  | ELambda (args,e)         -> compile_lambda   args e        si ds env
  | EPair  (e1,e2)           -> compile_pair     e1 e2         si ds env
  | ESeq ([e])               -> compile_expr     e             si ds env
  | ESeq (e::es)             -> compile_expr     e             si ds env
                              @ compile_expr     (ESeq es)     si ds env
  | ESeq []                  -> []

let compile_decl (ds : decl list) (d : decl) : instruction list =
  let DFun (fname, args, body) = d in
  let env = List.rev (List.mapi (fun i s -> (s, i+2)) args) in
  let si = List.length args + 2 in
  [ ILabel fname ] @ compile_expr body si ds env @ [ IRet ]

let rec well_formed_e (e : expr) (ds : decl list) (env : bool envt) =
  let go xs = flatmap (fun x -> well_formed_e x ds env) xs in
  match e with
    | ENumber _ | EBool _ -> []
    | EPair (e1, e2)      -> go [e1; e2]
    | EId(x) ->
       begin match find env x with
       | None -> ["Unbound variable identifier " ^ x]
       | Some(_) -> []
       end
    | EPrim1(op, e) -> go [e]
    | EPrim2(op, left, right) -> go [left; right]
    | EIf(cond, thn, els) -> go [cond; thn; els]

    | EApp(EId(f), args) -> 
       begin match find env f with
       | None -> begin match find_decl ds f with
                 | None -> ["Unbound variable identifier " ^ f] @ go args
                 | Some(DFun(fn,xs,_)) -> 
                    let arity = 
                      if List.length xs = List.length args then
                        []
                      else
                        ["Arity mismatch: " ^ fn]
                    in arity @ go args
                 end
       | Some(_) -> go args
       end
         
    | EApp(name, args)   -> go (name :: args)
      
    | ELambda(args, body) ->
       let dup_args =
         begin match find_dup args with
         | None -> []
         | Some(dup) -> ["Multiple arguments named " ^ dup]
         end
       in
       let body_env = List.map (fun a -> (a, true)) args in
       let from_body = well_formed_e body ds (body_env @ env) in
       dup_args @ from_body

    | ELet(binds, body) ->
      let names          = List.map fst binds in
      let env_from_binds = List.map (fun a -> (a, true)) names in
      let from_body      = well_formed_e body ds (env_from_binds @ env) in
      let dup_binds =
        begin match find_dup names with
        | None       -> []
        | Some(name) -> ["Multiple bindings for variable identifier " ^ name]
        end
      in
      let (from_binds, _) =
        let f (errs, env) (name, be) =
          let errs' = well_formed_e be ds env in
          let env'  = (name,true)::env in
          (errs @ errs', env') in
        List.fold_left f ([], env) binds
      in
      dup_binds @ from_binds @ from_body
    | ESeq es -> go es


let well_formed_d (d : decl) (ds : decl list) : string list =
  match d with
  | DFun(name, args, body) ->
     let env = List.map (fun a -> (a, true)) args in
     let from_body = well_formed_e body ds env in
     begin match find_dup args with
     | None -> from_body
     | Some(v) -> ("Duplicate parameter " ^ v)::from_body
     end

let well_formed_p (p : program) : string list =
  match p with
  | Program(ds, maine) ->
     let names = List.map (fun (DFun(name, _, _)) -> name) ds in
     let subexpr_errs = (well_formed_e maine ds [])
                      @ (flatmap (fun d -> well_formed_d d ds) ds)
     in
     begin match find_dup names with
     | None -> subexpr_errs
     | Some(v) -> ("Duplicate function " ^ v)::subexpr_errs
     end

let compile_to_string prog =
  match well_formed_p prog with
  | _::_ as errs ->
     failwith (unlines errs ^ "\n")
  | [] ->
     match prog with
     | Program(decls, main) ->
        let compiled_decls = flatmap (compile_decl decls) decls in
        let compiled_main = compile_expr main 1 decls [] in
        let prelude = unlines
          [ "section .text"
          ; "extern error"
          ; "extern equal"
          ; "extern print"
          ; "extern input"
          ; "global our_code_starts_here"
          ]
        in
        let main_start =
          [ ILabel("our_code_starts_here")
          ; IMov(Reg(EBX), RegOffset(4, ESP))
          ; IAdd(Reg(EBX), Const(8))
          ; IAnd(Reg(EBX), HexConst(0xFFFFFFF8))
          ]
        in
        let postlude
          = [ IRet ]
          @ [ ILabel error_non_int   ] @ (throw_err 1)
          @ [ ILabel error_non_bool  ] @ (throw_err 2)
          @ [ ILabel error_overflow  ] @ (throw_err 3)
          @ [ ILabel error_non_pair ] @ (throw_err 4)
          @ [ ILabel error_too_small ] @ (throw_err 5)
          @ [ ILabel error_too_large ] @ (throw_err 6)
          @ [ ILabel error_non_closure ] @ (throw_err 7)
          @ [ ILabel error_arity_mismatch] @ (throw_err 8)
        in
        let as_assembly_string =
          [ compiled_decls
          ; main_start
          ; compiled_main
          ; postlude
          ] |> List.flatten |> to_asm
        in
        unlines [prelude; as_assembly_string; "\n"]
