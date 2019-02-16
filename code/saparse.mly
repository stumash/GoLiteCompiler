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

/*KeyWord Tokens */

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

%token PRINTLN
%token PRINT
%token APPEND
%token LEN
%token CAP

%token LPAREN RPAREN (* (  ) *)
%token LBRACK RBRACK (* [ ] *)
%token LCURLY RCURLY (* { } *)

%token SEMICOLON (* ; *)
%token COLON (* : *)
%token COMMA (* , *)
%token DOT (* . *)

%token ELLIPSIS (* ... *)

%token <int> INT
(*
%token PLUS MINUS TIMES DIV
*)
%token EOF (* end of file *)

(*decide whether we use precedence directives or just normal and create our grammer to be correct *)
(*
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
*)

(* Arithmetic operators *)
%token PLUS MINUS (* '+' '-' *)
%token MULT DIV (* '*' '/' *)
%token MOD (* '%' *)

(* Logical Operators *)
%token AND OR (* '&' '|' *)
%token XOR (* '^' *)
%token LSHFT RSHFT (* '<<' '>>' *)
%token NAND (* '&^' *)
%token NOT (* '!' *)

(* Shorthand Arithmetic *)
%token SPLUS SMINUS (* '+=' '-=' *)
%token SMULT SDIV (* '*=' '/=' *)
%token SMOD (* '%=' *)

(* Shorthand Logical *)
%token SAND SOR (* '&=' '|=' *)
%token SXOR (* '^=' *)
%token SLSHFT SRSHFT (* '<<=' '>>=' *)
%token SNAND (* '&^=' *)

(* Relational Operators *)
%token EQ NEQ (* '==' '!=' *)
%token LT LTEQ (* '<' '<=' *)
%token GT GTEQ (* '>' '>=' *)
%token ANDAND OROR (* '&&' '||' *)

(* Misc Operators *)
%token CH (* '<-' *)
%token INC DEC (* '++' '--' *)
%token ASS (* '=' *)
%token COLONASS (* need to define a name appropriately for ':=' as i yet do not know what it signifies *)



/* changed the type, because the script does not return one value, but all
 * results which are calculated in the file */
%start <int list> main

%%

/* the calculated results are accumalted in an OCaml int list */
main:
| stmt = statement EOF { [stmt] }
| stmt = statement m = main { stmt :: m}

/* expressions end with a semicolon, not with a newline character */
statement:
| e = expr SEMICOLON { e }

expr:
| i = INT
    { i }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr PLUS e2 = expr
    { e1 + e2 }
| e1 = expr MINUS e2 = expr
    { e1 - e2 }
| e1 = expr MULT e2 = expr
    { e1 * e2 }
| e1 = expr DIV e2 = expr
    { e1 / e2 }
