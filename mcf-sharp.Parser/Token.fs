
module Token



type Token = 
    | TIdentifier of string
    | TString of string
    | TInt of int
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