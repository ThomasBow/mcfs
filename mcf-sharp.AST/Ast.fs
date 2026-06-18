
module Ast

type BinaryOperatorKind =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Equals
    | NotEquals
    | LessThan
    | GreaterThan
    | LessThanOrEqual
    | GreaterThanOrEqual

type Expression = 
    | IntLiteral of int
    | Variable of string
    | BinaryOperator of op: BinaryOperatorKind * left: Expression * right: Expression
    | Call of functionName: string * arguments: Expression list

type Statement =
    | VariableDeclaration of name: string * initializer: Expression option
    | VariableAssignment of name: string * value: Expression
    | If of condition: Expression * thenBranch: Statement list * elseBranch: Statement list option
    | While of condition: Expression * body: Statement list
    | FunctionCall of functionName: string * arguments: Expression list
    | Return of value: Expression option

type Parameter = 
    { Name: string; Type: string }

type FunctionDefinition =
    { Name: string; Parameters: string list; Body: Statement list}

type Tag =
    | Load
    | Tick

type TaggedBlock =
    {Tag: Tag; Statements: Statement list}

type Program =
    { Functions: FunctionDefinition list; TaggedBlocks: TaggedBlock list}
