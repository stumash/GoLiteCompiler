(* Abstract Syntax Tree definition *)

type prog =
  | Program of package * (declaration list) 
  | EmptyProgram

and package =
  | Package of string

and declaration =
  | FunctionDeclaration of identifier * parameters * (type_spec option) * (statement list)
  | VariableDeclaration of ((identifier list) * (type_spec option) * (expression list option)) list
  | TypeDeclaration of (identifier * type_spec) list

and identifier =
  | Identifier of string

and type_spec =
  | IdentifierType of identifier
  | ArrayTypeLiteral of expression * type_spec
  | StructTypeLiteral of (identifier * type_spec) list
  | SliceTypeLiteral of type_spec

and parameters =
  | Parameters of ((identifier list) * type_spec) list

and statement =
  | ExpressionStatement of expression
  | AssignmentStatement of (expression list) * assignment_operator * (expression list)
  | DeclarationStatement of declaration
  | ShortValDeclaration of (identifier list) * (expression list)
  | Inc of expression
  | Dec of expression
  | PrintStatement of (expression list option)
  | PrintlnStatement of (expression list option)
  | ReturnStatement of expression option
  | IfStatement of if_statement
  | SwitchStatement of statement * (expression option) * (switch_clause list)
  | ForStatement of statement * (expression option) * statement * (statement list)
  | BlockStatements of (statement list)
  | Break
  | Continue
  | EmptyStatement

and if_statement =
  | If of statement * expression * (statement list) * (else_statement option)

and else_statement =
  | Elseif of if_statement
  | Else of statement list

and switch_clause =
  | Default of statement list
  | Case of (expression list) * (statement list)

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
  | FunctionCall of identifier  * (expression list)
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
  | LitRawString of string
  | IdentifierExpression of identifier_expression

and identifier_expression =
  | Ident of string
  | Indexed of expression * expression
  | StructAccess of expression * string
