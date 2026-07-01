
module Ast

type Position =
    { Line: int; Column: int }

type Node<'a> =
    { Value: 'a; Position: Position }

type Type =
    | TypeInt
    | TypeBool
    | TypeVoid
    | TypeString
    | ErrorType

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
    | StringLiteral of string
    | Variable of string
    | BinaryOperator of op: BinaryOperatorKind * left: Node<Expression> * right: Node<Expression>
    | Call of functionName: string * arguments: Node<Expression> list
    | ErrorExpression

type Statement =
    | VariableDeclaration of name: string * typeHint: Type option * initializer: Node<Expression> option
    | VariableAssignment of name: string * value: Node<Expression>
    | If of condition: Node<Expression> * thenBranch: Node<Statement> list * elseBranch: Node<Statement> list option
    | While of condition: Node<Expression> * body: Node<Statement> list
    | FunctionCall of functionName: string * arguments: Node<Expression> list
    | Return of value: Node<Expression> option
    | RawCommand of command: string
    | ErrorStatement

type Parameter = 
    { Name: string; Type: Type }

type FunctionDefinition =
    { Name: string; Parameters: Parameter list; Body: Node<Statement> list; ReturnType: Type }

type Tag =
    | Load
    | Tick
    | ErrorTag

type TaggedBlock =
    {Tag: Tag; Statements: Node<Statement> list }

type Program =
    { Functions: FunctionDefinition list; TaggedBlocks: TaggedBlock list}
