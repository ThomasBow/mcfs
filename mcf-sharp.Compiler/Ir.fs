



module Ir

type ComparisonOperator =
    | IrLessThan
    | IrLessThanOrEqual
    | IrGreaterThan
    | IrGreaterThanOrEqual
    | IrEquals
    | IrNotEquals

type NBTValue = 
    | NBTByte       of int8
    | NBTShort      of int16
    | NBTInt        of int32
    | NBTLong       of int64
    | NBTFloat      of float32
    | NBTDouble     of float
    | NBTString     of string
    | NBTList       of NBTValue list
    | NBTCompound   of Map<string, NBTValue>

type IrInstruction =
    | IrSetConstant         of destination: string * value: int
    | IrCopy                of destination: string * source: string
    | IrAdd                 of destination: string * source: string
    | IrSubtract            of destination: string * source: string
    | IrMultiply            of destination: string * source: string
    | IrDivide              of destination: string * source: string
    | IrModulo              of destination: string * source: string
    
    | IrStorageSet          of path: string * value: NBTValue
    | IrStorageGet          of destination: string * path: string
    
    | IrCall                of functionName: string
    | IrConditionalCall     of valueA: string * operator: ComparisonOperator * valueB: string * functionName: string
    | IrMacroCall           of functionName: string * storageArgumentPath: string

type IrFunction = 
    { Name: string; Instructions: IrInstruction list }

type IrProgram = 
    { Functions: IrFunction list }