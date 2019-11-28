open Printf

(* Abstract syntax of (a small subset of) x86 assembly instructions *)
type reg =
    | EAX
    | ESP

type arg =
    | Const of int
    | Reg of reg
    | RegOffset of int * reg

type instruction =
    | IMov of arg * arg
    | IAdd of arg * arg
    | IRet


(* Abstract syntax of the Adder language *)
type prim1 =
    | Add1
    | Sub1

type expr =
    | Number of int
    | Prim1 of prim1 * expr
    | Let of (string * expr) list * expr
    | Id of string


(* Functions that implement the compiler *)
let reg_to_asm_string (r : reg) : string =
    match r with
    | EAX -> "eax"
    | ESP -> "esp"

let arg_to_asm_string (a : arg) : string =
    match a with
    | Const(n) -> sprintf "%d" n
    | Reg(r) -> reg_to_asm_string r
    | RegOffset(n, r) -> sprintf "[%s + %d]" (reg_to_asm_string r) n

let instruction_to_asm_string (i : instruction) : string =
    match i with
    | IMov(dest, value) ->
            sprintf "mov %s, %s" (arg_to_asm_string dest) (arg_to_asm_string value)
    | IRet -> "ret"
    | IAdd(dest, to_add) -> sprintf "add %s, %s" (arg_to_asm_string dest) (arg_to_asm_string to_add)

let to_asm_string (ins : instruction list) : string =
    List.fold_left (fun s i -> sprintf "%s\n%s" s (instruction_to_asm_string i)) "" ins

let rec find (ls : (string * int) list) (x : string) =
    match ls with
    | [] -> None
    | (y,v)::rest ->
      if y = x then Some(v) else find rest x


let rec compile_env (p : expr) (stack_index : int) (env : (string * int) list) : instruction list =
    match p with
    | Number(n) -> [ IMov(Reg(EAX), Const(n)) ]
    | Prim1(op, e) ->
            (match op with
             | Add1 -> (compile_env e stack_index env) @ [ IAdd(Reg(EAX), Const(1)) ]
             | Sub1 -> (compile_env e stack_index env) @ [ IAdd(Reg(EAX), Const(-1)) ])
    | Id(x) ->
            (match find env x with
             | None -> failwith (sprintf "%s not in scope" x)
             | Some value -> [IMov(Reg(EAX), RegOffset(value, ESP))])
    | Let(binds, body) ->
            (match binds with
             | [] -> [] @ (compile_env body stack_index env)
             | (str, exp)::rest ->
                    let new_env = env @ [(str, -4*stack_index)] in
                    let eval_expr = (compile_env exp stack_index env) in
                    let eval_tail = (compile_env (Let(rest, body)) (stack_index+1) new_env) in
                    eval_expr @ [IMov(RegOffset(-4*stack_index, ESP), Reg(EAX))] @ eval_tail)


let compile (p : expr) : instruction list = compile_env p 1 []

(* The entry point for the compiler: a function that takes a expr and
creates an assembly-program string representing the compiled version *)
let compile_to_string (prog : expr) =
  let prelude =
      "section .text global our_code_starts_here our_code_starts_here:" in
  let asm_string = (to_asm_string ((compile prog) @ [IRet])) in
  let asm_program = sprintf "%s%s\n" prelude asm_string in
  asm_program
