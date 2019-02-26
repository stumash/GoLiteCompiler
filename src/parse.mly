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
    | PACKAGE IDENT { print_string "Package" }
    ;

declarations :
    | declarations declaration {  }
    | (* empty *) { print_string "empty declarations" }
    ;

declaration :
    | block_declaration { }
    | function_dec {  }
    ;

(* block declarations are allowed within any block, unlike function declarations *)
block_declaration :
    | variable_dec {  }
    | type_dec {  }
    ;


variable_dec :
    | VAR IDENT versions {  }
    ;

type_dec :
    | TYPE IDENT versions {  }
    | struct_def {  }
    ;

(* version to encompass all above mentioned stuff of types *)
(* We will need to take care of IDENT here as it can only be a struct, or a basic datatype or a typed type (wew the paradox! ) *)
versions:
    | IDENT (* struct types , basic types *)
    | LBRACK RBRACK IDENT  (* slice types *)
    | LBRACK LIT_INT RBRACK IDENT  (* array types *) { }
    ;

(* struct definiton  *)
struct_def :
    | TYPE STRUCT LCURLY struct_body RCURLY { }
    ;
struct_body:
    | { }
    | IDENT struct_body_2 { }
    ;
struct_body_2:
    | versions struct_body  { }
    | COMMA IDENT struct_body_2 { }
    ;

(* Changes made were to add frame and a TYPE after RPAREN to specify return type of functions *)
function_dec :
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
    | TYPE { print_string "some type "}
    ;

(*Print and print_ln staterments *)
print_statement :
    | PRINT LPAREN exp_list RPAREN
    | PRINTLN LPAREN exp_list RPAREN { print_string "Printing something " }
    ;

(*list of expressions *)
exp_list :
    | { (* Empty list *)}
    | exp
    | exp_list COMMA exp { print_string "List of expressions" }

(*For statements *)
for_loop :
    | FOR loop_type { print_string "For" }
    ;

(*The last tyep of loop we will need to decide as the first thign is an assigment statment and the last is also worth discussion *)
loop_type :
    | LCURLY statements RCURLY {print_string "Infinite loop"}
    | exp LCURLY statements RCURLY {print_string "While loop "}
    | statement SEMICOLON exp SEMICOLON statement  LCURLY statements RCURLY {print_string "Normal Loop"}
    ;

(* statements *)
statements :
    | statements statement SEMICOLON{  }
    | LCURLY statements RCURLY { } (*block level statements *)
    | { print_string "empty statements" }
    ;


ident_type : 
    | BLANKID { } (*Blank Identifier *)
    | IDENT { } (* Normal as is *)
    | IDENT LBRACK ident_type RBRACK { } (* array and slice element access *)
    | IDENT DOT ident_type { } (* Struct element access *)
;

ass_type :
    | ASG { } (*normal assignment *)
    | PLUSEQ 
    | MINUSEQ
    | MULTEQ
    | DIVEQ
    | MODEQ { } (*Arithmetic shorthand *)
    | BANDEQ
    | BOREQ
    | XOREQ
    | LSHFTEQ
    | RSHFTEQ
    | NANDEQ { } (*Bitwise shorthand *)
;


(*Left to add in statment (please chekc if i missed something):
    * IF
    * SWITCH
    * INCREAMENT / DECREEAMNET
    * SHORTHAND DECLARATIONS (havent seen so far )
    *)
statement :
    | for_loop { }
    | print_statement { }
    | RETURN exp  { }
    | BREAK { }
    | CONTINUE  { }
    | exp { }
    | block_declaration { }
    | exp_list ass_type exp_list { }
    | ident_type INC 
    | ident_type DEC { } (* Shorthand inc dec *)
    ;

(*Expression grammar NEED TO DECIDE WHATS TO BE DONE REGARDING TYPE *)
(*Need to add function calls which will be defined after this *)
(* Need to change if expression is called by placing semicolon in statement before *)
exp :
    | exp_0 SEMICOLON {print_string "exp "}
    ;

exp_0 :
    | exp_0 OR exp_1
    | exp_1 { print_string "0" }
    ;

exp_1 :
    | exp_1 AND exp_2
    | exp_2 {print_string "1"}
    ;

exp_2 :
    | exp_2 EQ exp_3
    | exp_2 NEQ exp_3
    | exp_2 GT exp_3
    | exp_2 GTEQ exp_3
    | exp_2 LT exp_3
    | exp_2 LTEQ exp_3 {print_string "2"}
    ;

exp_3 :
    | exp_3 PLUS exp_4
    | exp_3 MINUS exp_4
    | exp_3 BOR exp_4
    | exp_3 XOR exp_4 {print_string "3"}
    ;

exp_4 :
    | exp_4 MULT exp_5
    | exp_4 DIV exp_5
    | exp_4 MOD exp_5
    | exp_4 LSHFT exp_5
    | exp_4 RSHFT exp_5
    | exp_4 BAND exp_5
    | exp_4 NAND exp_5 {print_string "4"}
    ;

exp_5 :
(*Unary based oeprations since the priority is the highest*)
    | PLUS exp_6
    | MINUS exp_6
    | NOT exp_6
    | XOR exp_6 {print_string "5"}
    | exp_6 {print_string "operand"}
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
    | ident_type {print_string "Identifier_variable"}
    | LIT_INT
    | LIT_BOOL
    | LIT_FLOAT
    | LIT_RUNE
    | LIT_STRING {print_string "Literal"}
    ;
