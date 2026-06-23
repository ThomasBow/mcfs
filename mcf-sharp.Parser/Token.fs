
module Token

open Ast

type Token = 
    | TIdentifier of string
    | TStringLiteral of string
    | TIntLiteral of int
    | TPlus
    | TMinus
    | TTimes
    | TDivide
    | TModulus
    | TLessThan
    | TLessThanEqual
    | TGreaterThan
    | TGreaterThanEqual
    | TEqual
    | TNotEqual
    | TComma
    | TSemicolon
    | TColon
    | TDoubleColon
    | TFunction
    | TIf
    | TElse
    | TWhile
    | TReturn
    | TSnabelA
    | TLeftBrace
    | TRightBrace
    | TLeftParenthesis
    | TRightParenthesis
    | TEndOfFile
    | TTick
    | TLoad
    | TMcf
    | TInt
    | TString
    | TVoid
    | TBool

type PositionedToken =
    { Token: Token; Position: Position }