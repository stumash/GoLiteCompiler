%{
    open Tree
%}

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
%nonassoc UNARY



(* change <unit> to <anytype> as needed *)
%start <unit> program


%%
(* GRAMMAR  *)

program :
    | package declarations EOF { print_endline "top level" }
    ;

package :
    | PACKAGE package_name=IDENT SEMICOLON { Printf.printf "package %s\n"  }
    ;

declarations :
    | ds=declarations d=declaration SEMICOLON  { d @ ds }
    | { [] } 
    ;

declaration :
    | d=block_declaration { [d] ; print_endline "GG VAR" }
    | d=function_declaration { [d] } (* function declarations cannot be in blocks *)
    ;

block_declaration :
    | variable_declaration { print_endline "GG VAR" }
    | type_declaration {  }
    ;

variable_declaration :
    | VAR variable_declaration_ { print_endline "GG VAR" }
;

variable_declaration_ :
    | var_spec { print_endline "VAR" }
    | LPAREN var_specs RPAREN {  }
;

var_specs :
    | { }
    | var_spec SEMICOLON var_specs { }
;

var_spec :
    | identifier_list type_spec option(var_spec_rhs) { print_endline "GG VAR" }
    | identifier_list option(var_spec_rhs) {  }
    ;
var_spec_rhs :
    | ASG expression_list { }
    ;

identifier_list :
    | separated_nonempty_list(COMMA, IDENT) {  }
    ;

type_spec :
    | IDENT { print_endline "GG IDENT" }
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
    | TYPE IDENT type_spec {  }
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
    | type_spec frame   { }
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
    | statement SEMICOLON statements (*SEMI*) {  }
    | statement_block {  }
    ;


ident_type :
    | e=BLANKID                        { Blankid } (*Blank Identifier *)
    | s=IDENT                          { Ident s } (* Normal as is *)
    | s=IDENT LBRACK o=operand RBRACK  { Indexed (s, o) } (* array and slice element access *)
    | s=IDENT DOT e2=ident_type        { StructAccess (s, e2) } (* Struct element access *)
    ;

asg_tok :
    | ASG     { ASG }
    | PLUSEQ  { PLUSEQ }
    | MINUSEQ { MINUSEQ }
    | MULTEQ  { MULTEQ }
    | DIVEQ   { DIVEQ }
    | MODEQ   { MODEQ }
    | BANDEQ  { BANDEQ }
    | BOREQ   { BOREQ }
    | XOREQ   { XOREQ }
    | LSHFTEQ { LSHFTEQ }
    | RSHFTEQ { RSHFTEQ }
    | NANDEQ  { NANDEQ }
    ;

statement :
    | e=exp                   { ExpressionStatement e }
    | s=assignment_statement  { s }
    | s=block_declaration     { s }
    | s=short_val_declaration { s }
    | s=inc_dec_statement     { s }
    | s=print_statement       { s }
    | RETURN e=exp            { ReturnStatement e }
    | s=if_statement          { s }
    | s=switch_statement      { s }
    | s=for_loop              { s }
    | BREAK                   { Break }
    | CONTINUE                { Continue }
    ;

simple_statement :
    | exp SEMICOLON                   { }
    | inc_dec_statement SEMICOLON     { }
    | assignment_statement SEMICOLON  { }
    | short_val_declaration SEMICOLON { }
    ;

statement_block :
    | LCURLY ss=statements RCURLY { ss }
    ;

assignment_statement :
    | es1=expression_list at=asg_tok es2=expression_list { AssignmentStatement (es1, at, es2) }
    ;

short_val_declaration :
    | es1=expression_list IASG es2=expression_list { ShortValDeclaration (es1, es2) }
    ;

inc_dec_statement :
    | exp INC { Inc e }
    | exp DEC { Dec e }
    ;

if_statement :
    | IF e=exp ss=statement_block en=endif { If (None, e, ss, en) }
    ;
endif :
    | ELSE i=if_statement { Some (Elseif i) }
    | ELSE ss=statement_block { Some (Else ss) }
    | { None }
    ;

switch_statement :
    | SWITCH  eo=option(exp) LCURLY
          scl=nonempty_list(expr_case_clause)
      RCURLY { SwitchStatement (None, eo, scl) }

expr_case_clause :
    | esc=expr_switch_case COLON ss=statements { if esc = None then Default ss else Case (esc, ss) }
    ;
expr_switch_case :
    | DEFAULT { None }
    | CASE es=expression_list { es }
    ;

expression_list :
    | es=separated_nonempty_list(COMMA, exp) { es }
    ;

exp :
(* binary operator expressions *)
    | e1=exp OR e2=exp         { Or (e1, e2) }
    | e1=exp AND e2=exp        { And (e1, e2) }
    | e1=exp EQ e2=exp         { Eq (e1, e2) }
    | e1=exp NEQ e2=exp        { Neq (e1, e2) }
    | e1=exp GT e2=exp         { Gt (e1, e2) }
    | e1=exp GTEQ e2=exp       { Gteq (e1, e2) }
    | e1=exp LT e2=exp         { Lt (e1, e2) }
    | e1=exp LTEQ e2=exp       { Lteq (e1, e2) }
    | e1=exp PLUS e2=exp       { Plus (e1, e2) }
    | e1=exp MINUS e2=exp      { Minus (e1, e2) }
    | e1=exp BOR e2=exp        { Bor (e1, e2) }
    | e1=exp XOR e2=exp        { Xor (e1, e2) }
    | e1=exp MULT e2=exp       { Mult (e1, e2) }
    | e1=exp DIV e2=exp        { Div (e1, e2) }
    | e1=exp MOD e2=exp        { Mod (e1, e2) }
    | e1=exp LSHFT e2=exp      { Lshft (e1, e2) }
    | e1=exp RSHFT e2=exp      { Rshft (e1, e2) }
    | e1=exp BAND e2=exp       { Band (e1, e2) }
    | e1=exp NAND e2=exp       { Nand (e1, e2) }
    | PLUS e1=exp %prec UNARY  { Uplus e1}
    | MINUS e1=exp %prec UNARY { Uminus e1}
    | NOT e1=exp %prec UNARY   { Not e1}
    | XOR e1=exp %prec UNARY   { Uxor e1 }
    | e=exp_other              { e }
    ;
(* 'keyword functions' *)
exp_other : 
    | e1=IDENT LPAREN e2=expression_list RPAREN { FunctionCall (e1, e2) } (*function calls *)
    | APPEND LPAREN e1=exp COMMA e2=exp RPAREN  { Append (e1, e2) } (* Append *)
    | LEN LPAREN e=exp RPAREN                   { Len e } (* array/slice length *)
    | CAP LPAREN e=exp RPAREN                   { Cap e } (* array/slice capacity *)
    | LPAREN e=exp RPAREN                       { ParenExpression e } (* '(' exp ')' *)
    | e=operand                                 { e }
    ;

(* identifiers and rvalues *)
operand :
    | e=ident_type { IdentifierExpression e }
    | e=LIT_INT    { LitInt e }
    | e=LIT_BOOL   { LitBool e }
    | e=LIT_FLOAT  { LitFloat e }
    | e=LIT_RUNE   { LitRune e }
    | e=LIT_STRING { LitString e }
    ;
