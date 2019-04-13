%{
    (* Add to contents of generated parse.ml *)

    open Tree
    open Weeder
    open Helpers

    let e_to_id = function
        | IdentifierExpression (Ident (str,pos)) -> Identifier (str,pos)
        | _ -> raise ExpressionIsNotIdentifier

    (* expression list to identifier list *)
    let es_to_ids es =
        List.map e_to_id es

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
        | ExpressionStatement (e,pos) ->
            (match e with
            | FunctionCall (Identifier (str,pos1), es, pos2) ->
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
%token <int*int> BREAK
%token <int*int> CASE
%token <int*int> CHAN
%token <int*int> CONST
%token <int*int> CONTINUE
%token <int*int> DEFAULT
%token <int*int> DEFER
%token <int*int> ELSE
%token <int*int> FALLTHROUGH
%token <int*int> FOR
%token <int*int> FUNC
%token <int*int> GO
%token <int*int> GOTO
%token <int*int> IF
%token <int*int> IMPORT
%token <int*int> INTERFACE
%token <int*int> MAP
%token <int*int> PACKAGE
%token <int*int> RANGE
%token <int*int> RETURN
%token <int*int> SELECT
%token <int*int> STRUCT
%token <int*int> SWITCH
%token <int*int> TYPE
%token <int*int> VAR

(* Keyword Functions *)
%token <int*int> PRINTLN
%token <int*int> PRINT
%token <int*int> APPEND
%token <int*int> LEN
%token <int*int> CAP

(* Punctuation *)
%token <int*int> LPAREN RPAREN (* (  ) *)
%token <int*int> LBRACK RBRACK (* [ ] *)
%token <int*int> LCURLY RCURLY (* { } *)

%token <int*int> SEMICOLON (* ; *)
%token <int*int> COLON (* : *)
%token <int*int> COMMA (* , *)
%token <int*int> DOT (* . *)
%token <int*int> ELLIPSIS (* ... *)

(* Arithmetic operators *)
%token <int*int> PLUS MINUS (* '+' '-' *)
%token <int*int> MULT DIV (* '*' '/' *)
%token <int*int> MOD (* '%' *)

(* Bitwise Operators *)
%token <int*int> BAND BOR (* '&' '|' *)
%token <int*int> XOR (* '^' *)
%token <int*int> LSHFT RSHFT (* '<<' '>>' *)
%token <int*int> NAND (* '&^' *)

(* Arithmetic Assignment *)
%token <int*int> PLUSEQ MINUSEQ (* '+=' '-=' *)
%token <int*int> MULTEQ DIVEQ (* '*=' '/=' *)
%token <int*int> MODEQ (* '%=' *)
%token <int*int> INC DEC (* '++' '--' *)

(* Bitwise Assignment (Shorthand version) *)
%token <int*int> BANDEQ BOREQ (* '&=' '|=' *)
%token <int*int> XOREQ (* '^=' *)
%token <int*int> LSHFTEQ RSHFTEQ (* '<<=' '>>=' *)
%token <int*int> NANDEQ (* '&^=' *)

(* Comparison Operators *)
%token <int*int> EQ NEQ (* '==' '!=' *)
%token <int*int> LT LTEQ (* '<' '<=' *)
%token <int*int> GT GTEQ (* '>' '>=' *)
%token <int*int> AND OR (* '&&' '||' *)
%token <int*int> NOT (* '!' *)

(* Assignment Operators *)
%token <int*int> CHASG (* '<-' *)
%token <int*int> ASG (* assign '=' *)
%token <int*int> IASG (* assign with inference ':=' *)

%token <int*int> BLANKID (* blank identifier '_' *)

(* Parametrized Tokens *)
%token <string*(int*int)> COMMENT
%token <string*(int*int)> IDENT
%token <int*(int*int)> LIT_INT
%token <float*(int*int)> LIT_FLOAT
%token <bool*(int*int)> LIT_BOOL
%token <string*(int*int)> LIT_RUNE
%token <string*(int*int)> LIT_STRING
%token <string*(int*int)> LIT_RAW_STRING

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
    | PACKAGE s_pos=IDENT SEMICOLON { let s,pos=s_pos in Package (s, pos) }
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
    | pos=VAR vd=variable_declaration_ { VariableDeclaration (vd, pos) }
    ;
variable_declaration_ :
    | vd=var_spec { [vd] }
    | LPAREN vds=var_specs RPAREN { vds }
    ;
var_specs :
    | { [] }
    | vd=var_spec SEMICOLON vds=var_specs { vd::vds }
    ;
var_spec :
    | ids=identifier_list tso=option(type_spec) eso=option(var_spec_rhs)
      {
          err_ifboth_None tso eso VarDecNeedsTypeOrInit;
          (match eso with
            | None -> ()
            | Some eso -> err_if_neq_len ids eso VarDecIdsLenNeqExpsLen);
          (ids, tso, eso)
      }
    ;
var_spec_rhs :
    | ASG es=expression_list { es }
    ;

type_declaration :
    | pos=TYPE td=type_declaration_ { TypeDeclaration (td,pos) }
    ;
type_declaration_ :
    | s_pos=IDENT ts=type_spec { let s,pos=s_pos in [(Identifier (s,pos), ts)] }
    | LPAREN s_tds=s_type_specs RPAREN { s_tds }
    ;
s_type_specs :
    | { [] }
    | s_pos=IDENT ts=type_spec SEMICOLON s_tds=s_type_specs { let s,pos=s_pos in (Identifier (s,pos), ts)::s_tds }
    ;
type_spec :
    | s_pos=IDENT { let s,pos=s_pos in IdentifierType (Identifier (s, pos)) }
    | tl=type_literal { tl }
    | LPAREN ts=type_spec RPAREN { ts }
    ;
type_literal :
    | atl=array_type_lit { atl }
    | strtl=struct_type_lit { strtl }
    | sltl=slice_type_lit { sltl }
    ;
array_type_lit :
    | pos=LBRACK i_pos=LIT_INT RBRACK ts=type_spec { let i,ipos=i_pos in ArrayTypeLiteral (LitInt (i,ipos), ts, pos) }
    ;
struct_type_lit :
    | pos=STRUCT LCURLY x=struct_field_decls  RCURLY { StructTypeLiteral (x, pos) }
    ;
struct_field_decls :
    | { [] }
    | ss=struct_field_decls  s=struct_field_decl SEMICOLON {s @ ss}

;
struct_field_decl :
    | ids=identifier_list ts=type_spec
      {
          List.combine ids (List.init (List.length ids) (fun i -> ts))
      }
    ;
slice_type_lit :
    | pos=LBRACK RBRACK ts=type_spec { SliceTypeLiteral (ts, pos) }
    ;

function_declaration :
    | pos1=FUNC s_pos=IDENT LPAREN p=params RPAREN ts=option(type_spec) LCURLY ss=statements RCURLY
      { let s,pos2=s_pos in FunctionDeclaration (Identifier (s, pos2), p, ts, ss, pos1) }
    ;

params :
    | p=separated_list(COMMA, ids_w_type) { Parameters p }
    ;

ids_w_type :
    | ids=identifier_list ts=type_spec { (ids, ts) }
    ;

(*Print and print_ln staterments *)
print_statement :
    | pos=PRINT LPAREN es=expression_list RPAREN   { PrintStatement (Some es, pos) }
    | pos=PRINTLN LPAREN es=expression_list RPAREN { PrintlnStatement (Some es, pos) }
    | pos=PRINT LPAREN RPAREN {PrintStatement (None, pos)}
    | pos=PRINTLN LPAREN RPAREN {PrintlnStatement (None, pos)}
    ;

(*For statements *)
for_loop :
    | pos=FOR fl=loop_type { let s1,eo,s2,ss,pos2=fl in ForStatement (s1, eo, s2, ss, pos, pos2) }
    ;

(*The last tyep of loop we will need to decide as the first thign is an assigment statment and the last is also worth discussion *)
loop_type :
    | s1=statement_ SEMICOLON eo=option(exp) pos=SEMICOLON s2=statement_ LCURLY ss=statements RCURLY
      {
          err_if ((not (is_simple_stmt s1)) || (not (is_simple_stmt s2))) NotSimpleStatement;
          (s1, eo, s2, ss, pos)
      }
    | pos=LCURLY ss=statements RCURLY
      {
          (EmptyStatement, None, EmptyStatement, ss, pos)
      }
    | e=exp pos=LCURLY ss=statements RCURLY
      {
          (EmptyStatement, Some e, EmptyStatement, ss, pos)
      }
    ;

(* statements *)
statements :
    | s=statement SEMICOLON ss=statements { s::ss }
    |                                     { [] }
    ;


ident_type :
    | s_pos=IDENT                                   { let s,pos=s_pos in Ident (s,pos) } (* Normal as is *)
    | LPAREN exp=exp RPAREN pos=LBRACK e=exp RBRACK { Indexed (exp, e, pos) } (* array and slice element access *)
    | exp_pos=funccall pos=LBRACK e=exp RBRACK      { let exp,_=exp_pos in Indexed (exp, e, pos) }
    | exp=ident_type pos=LBRACK e=exp RBRACK        { Indexed (IdentifierExpression exp, e, pos) }
    | LPAREN exp=exp RPAREN pos=DOT id_pos=IDENT    { let id,_=id_pos in StructAccess (exp, id, pos) } (* Struct element access *)
    | e_pos=funccall pos=DOT id_pos=IDENT           { let (e,_),(id,_)=e_pos,id_pos in StructAccess (e, id, pos) }
    | e=ident_type pos=DOT id_pos=IDENT             { let id,_=id_pos in StructAccess (IdentifierExpression e, id, pos) }
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
    | pos=LCURLY ss=statements RCURLY { BlockStatements (ss,pos) }

statement_ :
    | e_pos=funccall          { let e,pos=e_pos in ExpressionStatement (e, pos) }
    | s=assignment_statement  { s }
    | d=block_declaration     { DeclarationStatement d }
    | s=short_val_declaration { s }
    | s=inc_dec_statement     { s }
    | s=print_statement       { s }
    | pos=RETURN eo=option(exp)   { ReturnStatement (eo, pos) }
    | ifs=if_statement        { IfStatement ifs }
    | s=switch_statement      { s }
    | s=for_loop              { s }
    | pos=BREAK               { Break pos }
    | pos=CONTINUE            { Continue pos }
    |                         { EmptyStatement }
    ;

assignment_statement :
    | es1=expression_list at=asg_tok es2=expression_list
      {
          err_if_neq_len es1 es2 VarDecIdsLenNeqExpsLen;
          err_if ((List.length es1) != 1 && at != ASG) MultAsgCannotShorthand;
          AssignmentStatement (es1, at, es2)
      }
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
    | pos=IF e=exp LCURLY ss=statements RCURLY en=endif
      {
          If (EmptyStatement, e, ss, en, pos)
      }
    | pos=IF s=statement_ SEMICOLON e=exp LCURLY ss=statements RCURLY en=endif
      {
          err_if (not (is_simple_stmt s)) NotSimpleStatement;
          If (s, e, ss, en, pos)
      }
    ;
endif :
    | pos=ELSE ifs=if_statement            { Some (Elseif (ifs, pos)) }
    | pos=ELSE LCURLY ss=statements RCURLY { Some (Else (ss, pos)) }
    |                                      { None }
    ;

switch_statement :
    | pos=SWITCH eo=option(exp) LCURLY
          scl=list(expr_case_clause)
      RCURLY
      {
          let f acc sc = match sc with | Default (ss,_) -> acc+1 | _ -> acc in
          err_if (1 < List.fold_left f 0 scl) SwitchMultipleDefaults;
          SwitchStatement (EmptyStatement, eo, scl, pos)
      }
    | pos=SWITCH s=statement_ SEMICOLON eo=option(exp) LCURLY
          scl=list(expr_case_clause)
      RCURLY
      {
          let f acc sc = match sc with | Default (ss,_) -> acc+1 | _ -> acc in
          err_if (1 < List.fold_left f 0 scl) SwitchMultipleDefaults;
          err_if (not (is_simple_stmt s)) NotSimpleStatement;
          SwitchStatement (s, eo, scl, pos)
      }

expr_case_clause :
    | esc=expr_switch_case COLON ss=statements
      {
          match esc with
          | None,    pos -> Default (ss, pos)
          | Some es, pos -> Case (es, ss, pos)
      }
    ;
expr_switch_case :
    | pos=DEFAULT                 { None, pos }
    | pos=CASE es=expression_list { Some es, pos }
    ;

identifier_list :
    | ids=separated_nonempty_list(COMMA, IDENT) { List.map (fun (s,pos) -> Identifier (s,pos)) (List.rev ids) }
    ;


expression_list :
    | es=separated_nonempty_list(COMMA, exp) { es }
    ;

exp :
(* binary operator expressions *)
    | e1=exp pos=OR e2=exp         { Or (e1, e2, pos) }
    | e1=exp pos=AND e2=exp        { And (e1, e2, pos) }
    | e1=exp pos=EQ e2=exp         { Eq (e1, e2, pos) }
    | e1=exp pos=NEQ e2=exp        { Neq (e1, e2, pos) }
    | e1=exp pos=GT e2=exp         { Gt (e1, e2, pos) }
    | e1=exp pos=GTEQ e2=exp       { Gteq (e1, e2, pos) }
    | e1=exp pos=LT e2=exp         { Lt (e1, e2, pos) }
    | e1=exp pos=LTEQ e2=exp       { Lteq (e1, e2, pos) }
    | e1=exp pos=PLUS e2=exp       { Plus (e1, e2, pos) }
    | e1=exp pos=MINUS e2=exp      { Minus (e1, e2, pos) }
    | e1=exp pos=BOR e2=exp        { Bor (e1, e2, pos) }
    | e1=exp pos=XOR e2=exp        { Xor (e1, e2, pos) }
    | e1=exp pos=MULT e2=exp       { Mult (e1, e2, pos) }
    | e1=exp pos=DIV e2=exp        { Div (e1, e2, pos) }
    | e1=exp pos=MOD e2=exp        { Mod (e1, e2, pos) }
    | e1=exp pos=LSHFT e2=exp      { Lshft (e1, e2, pos) }
    | e1=exp pos=RSHFT e2=exp      { Rshft (e1, e2, pos) }
    | e1=exp pos=BAND e2=exp       { Band (e1, e2, pos) }
    | e1=exp pos=NAND e2=exp       { Nand (e1, e2, pos) }
    | pos=PLUS e1=exp %prec UNARY  { Uplus (e1, pos) }
    | pos=MINUS e1=exp %prec UNARY { Uminus (e1, pos) }
    | pos=NOT e1=exp %prec UNARY   { Not (e1, pos) }
    | pos=XOR e1=exp %prec UNARY   { Uxor (e1, pos) }
    | e=exp_other              { e }
    ;
(* 'keyword functions' *)
exp_other :
    | e_pos=funccall                               { let e,pos=e_pos in e }
    | pos=APPEND LPAREN e1=exp COMMA e2=exp RPAREN { Append (e1, e2, pos) } (* Append *)
    | pos=LEN LPAREN e=exp RPAREN                  { Len (e, pos) } (* array/slice length *)
    | pos=CAP LPAREN e=exp RPAREN                  { Cap (e, pos) } (* array/slice capacity *)
    | pos=LPAREN e=exp RPAREN                      { ParenExpression (e, pos) } (* '(' exp ')' *)
    | e=operand                                    { e }
    ;

funccall :
    | s_pos=IDENT LPAREN es=separated_list(COMMA, exp) RPAREN             { let s,pos=s_pos in (FunctionCall (Identifier (s,pos), es, pos)), pos }
    | pos=LPAREN e=exp RPAREN LPAREN es=separated_list(COMMA, exp) RPAREN { (FunctionCall ((e_to_id e), es, pos)), pos }
    ;

(* identifiers and rvalues *)
operand :
    | idexp=ident_type   { IdentifierExpression idexp }
    | i_pos=LIT_INT          { let i,pos=i_pos in LitInt (i, pos) }
    | b_pos=LIT_BOOL         { let b,pos=b_pos in LitBool (b, pos) }
    | f_pos=LIT_FLOAT        { let f,pos=f_pos in LitFloat (f, pos) }
    | str_pos=LIT_RUNE       { let str,pos=str_pos in LitRune (str, pos) }
    | str_pos=LIT_STRING     { let str,pos=str_pos in LitString (str, pos) }
    | str_pos=LIT_RAW_STRING { let str,pos=str_pos in LitRawString (str, pos) }
    ;
