/**************************************************************************/
/*                                                                        */
/*  Menhir                                                                */
/*                                                                        */
/*  François Pottier, INRIA Rocquencourt                                  */
/*  Yann Régis-Gianas, PPS, Université Paris Diderot                      */
/*                                                                        */
/*  Copyright 2005-2008 Institut National de Recherche en Informatique    */
/*  et en Automatique. All rights reserved. This file is distributed      */
/*  under the terms of the Q Public License version 1.0, with the change  */
/*  described in file LICENSE.                                            */
/*                                                                        */
/**************************************************************************/

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

(* Bitwise Assignment *)
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
%token <int> LITINT
%token <float> LITFLOAT
%token <bool> LITBOOL
%token <string> LITRUNE
%token <string> LITSTRING

(* end-of-file, required *)
%token EOF

(* Operator Precedence *)
%left OR
%left AND
%left EQ NEQ GT GTEQ LT LTEQ
%left PLUS MINUS LOR XOR
%left MULT DIV MOD LSHFT RSHFT LAND NAND


/* changed the type, because the script does not return one value, but all
 * results which are calculated in the file */

(* %start <int list> main *) (*Notice the int list type for the program this would be essential when we start building AST *)

(*have set it as UNit for now  need to change if required*)
%start <unit> program


%%
/* the calculated results are accumalted in an OCaml int list */

(* CODE WILL NOT COMPILE *)

(* WOULD SUGGEST TESTING THE SCANNER SEPARATE  *)

program :
    | package { print_string "package"}
    (*| declarations { print_string "declarations "}*)
    (*| misc {print_string "misc"} print statemtns as well as append (remove if not required later )*)
;

package :
   | PACKAGE IDENT {print_string "Package "}
;

(*Incomplete *)
(*declaration :*)
   (*| variable_dec*)
   (*| type_dec*)
   (*| function_dec*)

(*Put variable declarations here  *)


(*Put type declarations here *)


(* I will put function declaration here *)


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
    | exp_3 OR exp_4
    | exp_3 XOR exp_4 {print_string "3"}
;

exp_4 :
    | exp_4 MULT exp_5
    | exp_4 DIV exp_5
    | exp_4 MOD exp_5
    | exp_4 LSHFT exp_5
    | exp_4 RSHFT exp_5
    | exp_4 AND exp_5
    | exp_4 NAND exp_5 {print_string "4"}
;

exp_5 :
(*Unary based oeprations since the priority is the highest*)
    | PLUS exp_0
    | MINUS exp_0
    | NOT exp_0
    | XOR exp_0 {print_string "5"}
    | operand {print_string "operand"}
;

operand :
    | IDENT {print_string "identifier"}
    | LITINT
    | LITBOOL
    | LITFLOAT
    | LITRUNE
    | LITSTRING {print_string "Literal"}
;

(*Still need to add function here. Will do so after defining grammar for function calls *)
