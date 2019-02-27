(* TOKENS *)

(* Keywords *)
%token BREAK
%token CASE
%token CHAN
%token CONST
%token CONTINUE
%token DEFAULT
%token DEFER
%token ELSE
%token FALLTHROUGH
%token FOR
%token FUNC
%token GO
%token GOTO
%token IF
%token IMPORT
%token INTERFACE
%token MAP
%token PACKAGE
%token RANGE
%token RETURN
%token SELECT
%token STRUCT
%token SWITCH
%token TYPE
%token VAR

(* Keyword Functions *)
%token PRINTLN
%token PRINT
%token APPEND
%token LEN
%token CAP

(* Punctuation *)
%token LPAREN RPAREN (* (  ) *)
%token LBRACK RBRACK (* [ ] *)
%token LCURLY RCURLY (* { } *)

%token SEMICOLON (* ; *)
%token COLON (* : *)
%token COMMA (* , *)
%token DOT (* . *)
%token ELLIPSIS (* ... *)

(* Arithmetic operators *)
%token PLUS MINUS (* '+' '-' *)
%token MULT DIV (* '*' '/' *)
%token MOD (* '%' *)

(* Bitwise Operators *)
%token BAND BOR (* '&' '|' *)
%token XOR (* '^' *)
%token LSHFT RSHFT (* '<<' '>>' *)
%token NAND (* '&^' *)

(* Arithmetic Assignment *)
%token PLUSEQ MINUSEQ (* '+=' '-=' *)
%token MULTEQ DIVEQ (* '*=' '/=' *)
%token MODEQ (* '%=' *)
%token INC DEC (* '++' '--' *)

(* Bitwise Assignment (Shorthand version) *)
%token BANDEQ BOREQ (* '&=' '|=' *)
%token XOREQ (* '^=' *)
%token LSHFTEQ RSHFTEQ (* '<<=' '>>=' *)
%token NANDEQ (* '&^=' *)

(* Comparison Operators *)
%token EQ NEQ (* '==' '!=' *)
%token LT LTEQ (* '<' '<=' *)
%token GT GTEQ (* '>' '>=' *)
%token AND OR (* '&&' '||' *)
%token NOT (* '!' *)

(* Assignment Operators *)
%token CHASG (* '<-' *)
%token ASG (* assign '=' *)
%token IASG (* assign with inference ':=' *)

%token BLANKID (* blank identifier '_' *)

(* Parametrized Tokens *)
%token <string> COMMENT
%token <string> IDENT
%token <int> LIT_INT
%token <float> LIT_FLOAT
%token <bool> LIT_BOOL
%token <string> LIT_RUNE
%token <string> LIT_STRING

(* end-of-file, required *)
%token EOF

(* Operator Precedence *)
%left OR
%left AND
%left EQ NEQ GT GTEQ LT LTEQ
%left PLUS MINUS BOR XOR
%left MULT DIV MOD LSHFT RSHFT BAND NAND

(* change <unit> to <anytype> as needed *)
%start <unit> program


%%
(* GRAMMAR  *)

program :
    | package declarations EOF { print_endline "top level" }
    ;

package :
    | PACKAGE package_name=IDENT { Printf.printf "package %s\n" package_name }
    ;

declarations :
    | declarations declaration {  }
    | (* empty *) { print_endline "empty declarations" }
    ;

declaration :
    | block_declaration { }
    | function_declaration {  } (* function declarations cannot be in blocks *)
    ;

block_declaration :
    | variable_declaration {  }
    | type_declaration {  }
    ;

variable_declaration :
    | VAR variable_declaration_ {  }
    ;
variable_declaration_ :
    | var_spec SEMICOLON {  }
    | LPAREN separated_list(SEMICOLON, var_spec) option(SEMICOLON) RPAREN SEMICOLON {  }
    ;
var_spec :
    | identifier_list type_spec option(var_spec_rhs) {  } 
    | identifier_list option(var_spec_rhs) {  }
    ;
var_spec_rhs :
    | ASG expression_list {  }
    ;

identifier_list :
    | separated_nonempty_list(COMMA, IDENT) {  }
    ;

type_spec :
    | IDENT {  }
    | type_literal {  }
    ;
type_literal :
    | array_type_lit {  }
    | struct_type_lit {  }
    | slice_type_lit {  }
    ;
array_type_lit :
    | LBRACK exp RBRACK type_spec {  }
    ;
struct_type_lit :
    (* embedded types not supported? *)
    | STRUCT LCURLY separated_nonempty_list(SEMICOLON, struct_field_decl) RCURLY {  }
    ;
struct_field_decl :
    | identifier_list type_spec {  }
    ;
slice_type_lit :
    | LBRACK RBRACK type_spec {  }
    ;

type_declaration :
    | TYPE IDENT type_spec SEMICOLON {  }
    ;

(* version to encompass all above mentioned stuff of types *)
(* We will need to take care of IDENT here as it can only be a struct, or a basic datatype or a typed type (wew the paradox! ) *)
versions:
    | IDENT (* struct types , basic types *)
    | LBRACK RBRACK IDENT  (* slice types *)
    | LBRACK LIT_INT RBRACK IDENT  (* array types *) { }
    ;

(* Changes made were to add frame and a TYPE after RPAREN to specify return type of functions *)
function_declaration :
    | FUNC IDENT LPAREN frame RPAREN ret LCURLY statements RCURLY {  }
    ;

(*Frame is the set of parameters of the function *)
frame :
    | { (* No more parameters *)}
    | IDENT c { }
    ;


(* c is defined to take into account the two different styles to specify parameters *)
c :
    | versions frame   { }
    | COMMA IDENT c  { }
    ;

(* Function return type *)
ret :
    | {(* No return type *)}
    | TYPE { print_endline "some type "}
    ;

(*Print and print_ln staterments *)
print_statement :
    | PRINT LPAREN expression_list RPAREN
    | PRINTLN LPAREN expression_list RPAREN { print_endline "Printing something " }
    ;

(*For statements *)
for_loop :
    | FOR loop_type { print_endline "For" }
    ;

(*The last tyep of loop we will need to decide as the first thign is an assigment statment and the last is also worth discussion *)
loop_type :
    | LCURLY statements RCURLY {print_endline "Infinite loop"}
    | exp LCURLY statements RCURLY {print_endline "While loop "}
    | statement SEMICOLON exp SEMICOLON statement  LCURLY statements RCURLY {print_endline "Normal Loop"}
    ;

(* statements *)
statements :
    | statements statement {  }
    | (* empty *) {  }
    ;


ident_type :
    | BLANKID { } (*Blank Identifier *)
    | IDENT { } (* Normal as is *)
    | IDENT LBRACK ident_type RBRACK { } (* array and slice element access *)
    | IDENT DOT ident_type { } (* Struct element access *)
    ;

asg_tok :
    | ASG
    | PLUSEQ
    | MINUSEQ
    | MULTEQ
    | DIVEQ
    | MODEQ
    | BANDEQ
    | BOREQ
    | XOREQ
    | LSHFTEQ
    | RSHFTEQ
    | NANDEQ {}
    ;

statement :
    | statement_block {  }
    | exp { }
    | assignment_statement {  }
    | block_declaration {  }
    | short_val_declaration {  }
    | inc_dec_statement {  }
    | print_statement {  }
    | RETURN exp  {  }
    | if_statement {  }
    | switch_statement {  }
    | for_loop {  }
    | BREAK {  }
    | CONTINUE {  }
    ;

simple_statement :
    | SEMICOLON {  }
    | exp SEMICOLON {  }
    | inc_dec_statement {  }
    | assignment_statement {  }
    | short_val_declaration {  }
    ;

statement_block :
    | LCURLY statements RCURLY {  }
    ;

assignment_statement :
    | expression_list asg_tok expression_list SEMICOLON {  }
    ;

short_val_declaration :
    | identifier_list IASG expression_list SEMICOLON {  }
    ;

inc_dec_statement :
    | exp INC SEMICOLON {  }
    | exp DEC SEMICOLON {  }
    ;

if_statement :
    | IF option(simple_statement) statement_block ELSE endif {  }
    ;
endif :
    | if_statement {  }
    | statement_block {  }
    ;

switch_statement :
    | SWITCH option(simple_statement) option(exp) LCURLY expr_case_clauses RCURLY {  }
    ;
expr_case_clauses :
    | expr_case_clauses expr_case_clause {  }
    ;
expr_case_clause :
    | expr_switch_case COLON statements {  }
    ;
expr_switch_case :
    | DEFAULT {  }
    | CASE expression_list {  }
    ;

expression_list :
    | separated_nonempty_list(COMMA, exp) {  }
    ;

(*Expression grammar NEED TO DECIDE WHATS TO BE DONE REGARDING TYPE *)
(*Need to add function calls which will be defined after this *)
(* Need to change if expression is called by placing semicolon in statement before *)
exp :
    | exp_0 SEMICOLON {print_endline "exp "}
    ;

exp_0 :
    | exp_0 OR exp_1
    | exp_1 { print_endline "0" }
    ;

exp_1 :
    | exp_1 AND exp_2
    | exp_2 {print_endline "1"}
    ;

exp_2 :
    | exp_2 EQ exp_3
    | exp_2 NEQ exp_3
    | exp_2 GT exp_3
    | exp_2 GTEQ exp_3
    | exp_2 LT exp_3
    | exp_2 LTEQ exp_3 {print_endline "2"}
    ;

exp_3 :
    | exp_3 PLUS exp_4
    | exp_3 MINUS exp_4
    | exp_3 BOR exp_4
    | exp_3 XOR exp_4 {print_endline "3"}
    ;

exp_4 :
    | exp_4 MULT exp_5
    | exp_4 DIV exp_5
    | exp_4 MOD exp_5
    | exp_4 LSHFT exp_5
    | exp_4 RSHFT exp_5
    | exp_4 BAND exp_5
    | exp_4 NAND exp_5 {print_endline "4"}
    ;

exp_5 :
(*Unary based oeprations since the priority is the highest*)
    | PLUS exp_6
    | MINUS exp_6
    | NOT exp_6
    | XOR exp_6 {print_endline "5"}
    | exp_6 {print_endline "operand"}
    ;

exp_6 :
    | IDENT LCURLY param RCURLY { } (*function calls *)
    | APPEND LCURLY exp COMMA exp RCURLY { } (* Append *)
    | LEN LCURLY exp RCURLY { }
    | CAP LCURLY exp RCURLY { }
    | exp_7 {}
    ;

param:
    | { }
    | exp d { }
    ;

d :
    | { }
    | COMMA exp d { }
    ;

exp_7 :
    | LCURLY exp RCURLY { }
    | operand { }
    ;

operand :
    | ident_type {print_endline "Identifier_variable"}
    | LIT_INT
    | LIT_BOOL
    | LIT_FLOAT
    | LIT_RUNE
    | LIT_STRING {print_endline "Literal"}
    ;
