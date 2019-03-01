(*Tree definitions *)

type prog = 
  | Program of package * (declaration list)
  | EmptyProgram

and package =
  | Package of string

and declaration =
  | VariableDeclaration of identifier list * type_spec
  | TypeDeclaration of identifier * type_spec
  | FunctionDeclaration of identifier * parameters * type_spec * (statement list)

and identifier =
  | Identifier of string

and type_spec =
  | IdentifierType of identifier
  | ArrayTypeLiteral of expression * type_spec
  | StructTypeLiteral of (identifier list * type_spec) list
  | SliceTypeLiteral of type_spec

and parameters =
  | Parameters of (identifier list * type_spec) list

and statement =
  | ExpressionStatement of expression
  | AssignmentStatement of (expression list) * assignment_operator * (expression list)
  | DeclarationStatement of declaration
  | ShortValDeclaration of (identifier list) * (expression list)
  | Inc of expression
  | Dec of expression
  | PrintStatement of expression
  | ReturnStatement of expression
  | IfStatement of if_statement
  | SwitchStatement of (statement option) * (expression option) * (switch_clause list)
  | ForStatement of (statement option) * (expression option) * (statement option) * (statement list)
  | Break
  | Continue

and if_statement =
  | If of (statement option) * expression * (statement list) * (else_statement option)

and else_statement =
  | Elseif of if_statement
  | Else of (statement list)

and assignment_operator =
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
  | NANDEQ

and switch_clause =
  | Default of statement list
  | Case of (expression list) * (statement list)

and expression =
(* binary expression *)
  | Or of expression * expression
  | And of expression * expression
  | Eq of expression * expression
  | Neq of expression * expression
  | Gt of expression * expression
  | Gteq of expression * expression
  | Lt of expression * expression
  | Lteq of expression * expression
  | Plus of expression * expression
  | Minus of expression * expression
  | Bor of expression * expression
  | Xor of expression * expression
  | Mult of expression * expression
  | Div of expression * expression
  | Mod of expression * expression
  | Lshft of expression * expression
  | Rshft of expression * expression
  | Band of expression * expression
  | Nand of expression * expression
(* unary expression *)
  | Uplus of expression
  | Uminus of expression
  | Not of expression
  | Uxor of expression
(* function calls *)
  | FunctionCall of string  * (expression list)
  | Append of expression * expression
  | Len of expression
  | Cap of expression
  | ParenExpression of expression
(* misc *)
  | LitInt of int
  | LitBool of bool
  | LitFloat of float
  | LitRune of string
  | LitString of string
  | IdentifierExpression of identifier_expression

and identifier_expression =
  | Ident of string
  | Blankid
  | Indexed of string * expression
  | StructAccess of string *  identifier_expression