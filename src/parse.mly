%{
    (* Add to contents of generated parse.ml *)

    open Tree
    open Parser_exceptions

    (* expression list to identifier list *)
    let es_to_ids es =
        let e_to_id = function
            | IdentifierExpression (Ident str) -> Identifier str
            | _ -> raise ExpressionIsNotIdentifier in
        List.map e_to_id es

    let err_if b err = if b then raise err else ()

    (* raise err if o1 and o2 are None *)
    let err_ifboth_None o1 o2 err =
        match o1,o2 with
        | None,None -> raise err
        | _,_ -> ()

    (* raise err if l1 and l2 differ in length *)
    let err_if_neq_len l1 l2 err =
        err_if ((List.length l1) != (List.length l2)) err
  
    let is_simple_stmt stmt =
        match stmt with
        | ExpressionStatement e ->
            (match e with
            | FunctionCall (str, es) ->
                (match str with
                | "append" | "len" | "cap" -> false
                | _ -> true)
            | _ -> true)
        | EmptyStatement        -> true
        | ShortValDeclaration _ -> true
        | AssignmentStatement _ -> true
        | Inc _ | Dec _         -> true
        | _                     -> false
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
%token <string> LIT_RAW_STRING

(* end-of-file, required *)
%token EOF

(* Operator Precedence *)
%left OR
%left AND
%left EQ NEQ GT GTEQ LT LTEQ
%left PLUS MINUS BOR XOR
%left MULT DIV MOD LSHFT RSHFT BAND NAND
%nonassoc UNARY

(* start production and return type of parser *)
%start <Tree.prog> program


%%
(* GRAMMAR  *)

program :
    | p=package ds=declarations EOF { Program (p, ds) }
    | EOF { EmptyProgram }
    ;

package :
    | PACKAGE s=IDENT SEMICOLON { Package s }
    ;

declarations :
    | ds=declarations d=declaration SEMICOLON  { ds @ [d] }
    | { [] }
    ;

declaration :
    | d=block_declaration { d }
    | d=function_declaration { d } (* function declarations cannot be in blocks *)
    ;

block_declaration :
    | vd=variable_declaration { vd }
    | td=type_declaration { td }
    ;

variable_declaration :
    | VAR vd=variable_declaration_ { vd }
    ;
variable_declaration_ :
    | vd=var_spec { VariableDeclaration [vd] }
    | LPAREN vds=var_specs RPAREN { VariableDeclaration vds }
    ;
var_specs :
    | { [] }
    | vd=var_spec SEMICOLON vds=var_specs { vd::vds }
    ;
var_spec :
    | ids=identifier_list tso=option(type_spec) eso=option(var_spec_rhs)
      {
          err_ifboth_None tso eso VarDecNeedsTypeOrInit;
          (match eso with | None -> () | Some es -> err_if_neq_len ids es VarDecIdsLenNeqExpsLen);
          (ids, tso, eso)
      }
    ;
var_spec_rhs :
    | ASG es=expression_list { es }
    ;

type_declaration :
    | TYPE td=type_declaration_ { td }
    ;
type_declaration_ :
    | s=IDENT ts=type_spec { TypeDeclaration [(Identifier s, ts)] }
    | LPAREN s_tds=s_type_specs RPAREN { TypeDeclaration s_tds }
    ;
s_type_specs :
    | { [] }
    | s=IDENT td=type_spec SEMICOLON s_tds=s_type_specs { (Identifier s, td)::s_tds }
    ;
type_spec :
    | s=IDENT { IdentifierType (Identifier s) }
    | tl=type_literal { tl }
    ;
type_literal :
    | atl=array_type_lit { atl }
    | strtl=struct_type_lit { strtl }
    | sltl=slice_type_lit { sltl }
    ;
array_type_lit :
    | LBRACK e=exp RBRACK ts=type_spec { ArrayTypeLiteral (e, ts) }
    ;
struct_type_lit :
    | STRUCT LCURLY x=separated_nonempty_list(SEMICOLON, struct_field_decl) RCURLY { StructTypeLiteral x }
    ;
struct_field_decl :
    | ids=identifier_list ts=type_spec SEMICOLON{ (ids, ts) }
    ;
slice_type_lit :
    | LBRACK RBRACK ts=type_spec { SliceTypeLiteral ts }
    ;

function_declaration :
    | FUNC s=IDENT LPAREN p=params RPAREN ts=option(type_spec) LCURLY ss=statements RCURLY
      { FunctionDeclaration (Identifier s, p, ts, ss) }
    ;

params :
    | p=separated_list(COMMA, ids_w_type) { Parameters p }
    ;

ids_w_type :
    | ids=identifier_list ts=type_spec { (ids, ts) }
    ;

(*Print and print_ln staterments *)
print_statement :
    | PRINT LPAREN es=expression_list RPAREN   { PrintStatement (Some es) }
    | PRINTLN LPAREN es=expression_list RPAREN { PrintlnStatement (Some es) }
    | PRINT LPAREN RPAREN {PrintStatement None}
    | PRINTLN LPAREN RPAREN {PrintlnStatement None}
    ;

(*For statements *)
for_loop :
    | FOR fl=loop_type { fl }
    ;

(*The last tyep of loop we will need to decide as the first thign is an assigment statment and the last is also worth discussion *)
loop_type :
    | s1=statement_ SEMICOLON eo=option(exp) SEMICOLON s2=statement_ LCURLY ss=statements RCURLY
      {
          err_if ((not (is_simple_stmt s1)) || (not (is_simple_stmt s2))) NotSimpleStatement;
          ForStatement (s1, eo, s2, ss)
      }
    | LCURLY ss=statements RCURLY
      {
          ForStatement (EmptyStatement, None, EmptyStatement, ss)
      }
    | e=exp LCURLY ss=statements RCURLY
      {
          ForStatement (EmptyStatement, Some e, EmptyStatement, ss)
      }
    ;

(* statements *)
statements :
    | s=statement SEMICOLON ss=statements { s::ss }
    |                                     { [] } 
    ;

ident_type :
    | BLANKID                          { Blankid } (*Blank Identifier *)
    | s=IDENT                          { Ident s } (* Normal as is *)
    | s=IDENT LBRACK e=exp RBRACK  { Indexed (s, e) } (* array and slice element access *)
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
    | s=statement_ { s }
    | s=block_statement { s }
    ;

block_statement :
    | LCURLY ss=statements RCURLY { BlockStatements ss }

statement_ :
    | e=exp                   { ExpressionStatement e }
    | s=assignment_statement  { s }
    | d=block_declaration     { DeclarationStatement d }
    | s=short_val_declaration { s }
    | s=inc_dec_statement     { s }
    | s=print_statement       { s }
    | RETURN eo=option(exp)   { ReturnStatement eo }
    | ifs=if_statement        { IfStatement ifs }
    | s=switch_statement      { s }
    | s=for_loop              { s }
    | BREAK                   { Break }
    | CONTINUE                { Continue }
    |                         { EmptyStatement }
    ;

assignment_statement :
    | es1=expression_list at=asg_tok es2=expression_list { AssignmentStatement (es1, at, es2) }
    ;

short_val_declaration :
    | ids=expression_list IASG es=expression_list
      {
          err_if_neq_len ids es VarDecIdsLenNeqExpsLen;
          ShortValDeclaration (es_to_ids ids, es)
      }
    ;

inc_dec_statement :
    | e=exp INC { Inc e }
    | e=exp DEC { Dec e }
    ;

if_statement :
    | IF e=exp LCURLY ss=statements RCURLY en=endif
      {
          If (EmptyStatement, e, ss, en)
      }
    | IF s=statement_ SEMICOLON e=exp LCURLY ss=statements RCURLY en=endif
      {
          err_if (not (is_simple_stmt s)) NotSimpleStatement;
          If (s, e, ss, en)
      }
    ;
endif :
    | ELSE ifs=if_statement                 { Some (Elseif ifs) }
    | ELSE LCURLY ss=statements RCURLY { Some (Else ss) }
    |                                       { None }
    ;

switch_statement :
    | SWITCH eo=option(exp) LCURLY
          scl=list(expr_case_clause)
      RCURLY { SwitchStatement (EmptyStatement, eo, scl) }
    | SWITCH s=statement_ SEMICOLON eo=option(exp) LCURLY
          scl=list(expr_case_clause)
      RCURLY
      {
          err_if (not (is_simple_stmt s)) NotSimpleStatement;
          SwitchStatement (s, eo, scl)
      }

expr_case_clause :
    | esc=expr_switch_case COLON ss=statements
      {
          match esc with
          | None    -> Default ss
          | Some es -> Case (es, ss)
      }
    ;
expr_switch_case :
    | DEFAULT                 { None }
    | CASE es=expression_list { Some es }
    ;

identifier_list :
    | ids=separated_nonempty_list(COMMA, IDENT) { List.map (fun str -> Identifier str) ids }
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
    | str=IDENT LPAREN es=separated_list(COMMA, exp) RPAREN { FunctionCall (str, es) } (*function calls *)
    | APPEND LPAREN e1=exp COMMA e2=exp RPAREN   { Append (e1, e2) } (* Append *)
    | LEN LPAREN e=exp RPAREN                    { Len e } (* array/slice length *)
    | CAP LPAREN e=exp RPAREN                    { Cap e } (* array/slice capacity *)
    | LPAREN e=exp RPAREN                        { ParenExpression e } (* '(' exp ')' *)
    | e=operand                                  { e }
    ;

(* identifiers and rvalues *)
operand :
    | e=ident_type       { IdentifierExpression e }
    | i=LIT_INT          { LitInt i }
    | b=LIT_BOOL         { LitBool b }
    | f=LIT_FLOAT        { LitFloat f }
    | str=LIT_RUNE       { LitRune str }
    | str=LIT_STRING     { LitString str }
    | str=LIT_RAW_STRING { LitRawString str}
    ;
