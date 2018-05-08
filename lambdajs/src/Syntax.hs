module Syntax where

-- From LambdaJS/haskell/src/Syntax.hs

type Ident = String
type Label = String

data Op
    = ONumPlus
    | OStrPlus
    | OMul | ODiv | OMod | OSub
    | OLt  | OStrLt
    | OBAnd | OBOr | OBXOr | OBNot
    | OLShift | OSpRShift | OZfRShift
    | OStrictEq
    | OAbstractEq
    | OTypeof 
    -- ? | OSurfaceTypeof
    | OPrimToNum
    | OPrimToStr
    | OPrimToBool
    | OIsPrim
    | OHasOwnProp
    | OToInteger | OToInt32 | OToUInt32
    -- | OPrint -- ^for Rhino
    -- | OStrContains | OStrSplitRegExp | OStrSplitStrExp -- ^for Regexes
    | OStrStartsWith -- ^for forin
    | OStrLen
    -- | OObjIterHasNext | OObjIterNext | OObjIterKey -- ^more forin
    -- | OObjCanDelete
    | OMathExp | OMathLog | OMathCos | OMathSin | OMathAbs | OMathPow
    -- | ORegExpMatch | ORegExpQuote
    deriving (Show, Eq)

data Expr
    = ENumber Double
    | EString String
    | EBool Bool
    | EUndefined
    | ENull
    | ELambda [Ident] Expr
    | EObject [(String, Expr)]
    | EId Ident
    | EOp Op [Expr]
    | EApp Expr [Expr]
    | ELet [(Ident, Expr)] Expr
    | ESetRef Expr Expr
    -- | ERef Expr
    -- | EDeref Expr
    | EGetField Expr Expr
    | EUpdateField Expr Expr Expr
    | EDeleteField Expr Expr
    | ESeq Expr Expr
    | EIf Expr Expr Expr
    | EWhile Expr Expr
    | ELabel Label Expr
    | EBreak Label Expr
    | EThrow Expr
    | ECatch Expr Expr
    | EFinally Expr Expr
    -- An expression that calls eval, or a related function. If EEval becomes the active expression,
    -- our model immediately aborts.
    | EEval
    deriving (Show, Eq)

data Value
    = VNumber Double
    | VString String
    | VBool Bool
    | VUndefined
    | VNull
    | VLambda [Ident] Expr
    | VObject [(String, Value)]
    | VThrown Value
    | VBreak Label Value
    deriving (Show, Eq)

-- Questions
-- How to encode throws and breaks using arrows?
-- How to encode references?

data Type
    = TNumber
    | TString
    | TBool
    | TUndefined
    | TNull
    | TLambda [Ident] Type
    | TObject [(String, Type)]
    deriving (Show, Eq)

-- Questions
-- Should this include thrown and break types?
