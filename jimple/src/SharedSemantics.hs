{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module SharedSemantics where

import           Prelude hiding (rem,div,id,or,and,fail)

import           Data.List (find,elemIndex)

import           Control.Category

import           Control.Arrow
import           Control.Arrow.Environment
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import qualified Control.Arrow.Utils as U

import           Text.Printf
import           Syntax

type CanFail ex val c = (ArrowChoice c,ArrowFail (ex val) c,UseException ex val c)

type CanInterp env envval val ex bool c = (
  UseVal val c,
  UseBool bool val c,
  UseConst c,
  UseEnv (env String envval) c,
  CanFail ex val c,
  ArrowEnv String envval (env String envval) c,
  ArrowExcept EInvoke (Maybe val) (ex val) c,
  ArrowExcept ([Statement],[CatchClause]) (Maybe val) (ex val) c,
  ArrowFix [Statement] (Maybe val) c,
  ArrowReader ([Statement],[CatchClause]) c)

assert :: (CanFail e v c) => c (Bool,String) ()
assert = proc (prop,msg) -> if prop
  then returnA -< ()
  else failStatic -< msg

justOrFail :: (CanFail e v c) => c (Maybe x,String) x
justOrFail = proc (x,e) -> case x of
  Just v -> returnA -< v
  Nothing -> failStatic -< e

liftAMaybe :: ArrowChoice c => c x z -> c (Maybe x) (Maybe z)
liftAMaybe f = proc m -> case m of
  Just x -> f >>^ Just -< x
  Nothing -> returnA -< Nothing

getFieldSignatures :: ([Modifier] -> Bool) -> CompilationUnit -> [FieldSignature]
getFieldSignatures p unit =
  [fieldSignature unit m | FieldMember m <- fileBody unit, p (fieldModifiers m)]

readCompilationUnit :: (CanFail e v c,UseConst c) => c String CompilationUnit
readCompilationUnit = proc n -> do
  compilationUnits <- askCompilationUnits -< ()
  justOrFail -< (find (\u -> fileName u == n) compilationUnits,printf "CompilationUnit %s not loaded" (show n))

evalInvoke :: CanInterp env envval val ex bool c => c EInvoke (Maybe val)
evalInvoke = proc e -> case e of
  SpecialInvoke this m params -> runMethod -< (Just this,m,params)
  VirtualInvoke this m params -> runMethod -< (Just this,m,params)
  InterfaceInvoke this m params -> runMethod -< (Just this,m,params)
  StaticInvoke m params -> runMethod -< (Nothing,m,params)
  DynamicInvoke{} -> failStatic -< "DynamicInvoke is not implemented"

evalImmediate :: (ArrowChoice c,UseVal val c) => c Immediate val
evalImmediate = proc i -> case i of
  Local name -> readVar -< name
  DoubleConstant f -> doubleConstant -< f
  FloatConstant f -> floatConstant -< f
  IntConstant n -> intConstant -< n
  LongConstant f -> longConstant -< f
  NullConstant -> nullConstant -< ()
  StringConstant s -> stringConstant -< s
  ClassConstant c -> classConstant -< c

evalAtIdentifier :: (ArrowChoice c,UseVal val c) => c AtIdentifier val
evalAtIdentifier = proc i -> case i of
  ThisRef -> evalImmediate -< Local "@this"
  ParameterRef n -> evalImmediate -< Local ("@parameter" ++ show n)
  CaughtExceptionRef -> evalImmediate -< Local "@caughtexception"

evalBool :: (ArrowChoice c,UseBool bool val c,UseVal val c) => c BoolExpr bool
evalBool = proc (BoolExpr i1 op i2) -> do
  v1 <- evalImmediate -< i1
  v2 <- evalImmediate -< i2
  case op of
    Cmpeq -> eq -< (v1,v2)
    Cmpne -> neq -< (v1,v2)
    Cmpgt -> gt -< (v1,v2)
    Cmpge -> ge -< (v1,v2)
    Cmplt -> lt -< (v1,v2)
    Cmple -> le -< (v1,v2)

eval :: CanInterp env envval val ex bool c => c Expr val
eval = proc e -> case e of
  NewExpr t -> do
    assert -< (isBaseType t,"Expected a base type for new")
    newSimple -< t
  NewArrayExpr t i -> do
    assert -< (isNonvoidType t,"Expected a nonvoid type for newarray")
    v <- evalImmediate -< i
    newArray -< (t,[v])
  NewMultiArrayExpr t is -> do
    assert -< (isBaseType t,"Expected a nonvoid base type for newmultiarray")
    vs <- U.map evalImmediate -< is
    newArray -< (t,vs)
  InstanceOfExpr i t -> first evalImmediate >>> instanceOf -< (i,t)
  CastExpr t i -> first evalImmediate >>> (id &&& instanceOf) >>> cast -< (i,t)
  InvokeExpr invokeExpr -> do
    v <- tryCatch evalInvoke (U.pi2 >>> fail) -< invokeExpr
    justOrFail -< (v,"Method returned nothing")
  RefExpr refExpr -> case refExpr of
    ArrayRef l i -> (readVar *** evalImmediate) >>> readIndex -< (l,i)
    FieldRef l f -> first readVar >>> readField -< (l,f)
    SignatureRef f -> readStaticField -< f
  BinopExpr i1 op i2 -> do
    v1 <- evalImmediate -< i1
    v2 <- evalImmediate -< i2
    case op of
      And -> and -< (v1,v2)
      Or -> or -< (v1,v2)
      Xor -> xor -< (v1,v2)
      Rem -> rem -< (v1,v2)
      Cmp -> cmp -< (v1,v2)
      Cmpg -> cmpg -< (v1,v2)
      Cmpl -> cmpl -< (v1,v2)
      Shl -> shl -< (v1,v2)
      Shr -> shr -< (v1,v2)
      Ushr -> ushr -< (v1,v2)
      Plus -> plus -< (v1,v2)
      Minus -> minus -< (v1,v2)
      Mult -> mult -< (v1,v2)
      Div -> div -< (v1,v2)
  UnopExpr op i -> do
    v <- evalImmediate -< i
    case op of
      Lengthof -> lengthOf -< v
      Neg -> neg -< v
  ImmediateExpr i -> evalImmediate -< i
  MethodHandle _ -> failStatic -< "Evaluation of method handles is not implemented"

runStatements :: CanInterp env envval val ex bool c => c [Statement] (Maybe val)
runStatements = fix $ \run -> proc stmts -> case stmts of
  [] -> returnA -< Nothing
  (stmt:rest) -> case stmt of
    Label labelName -> do
      (_,cs) <- ask -< ()
      let clauses = filter (\c -> fromLabel c == labelName && Label (toLabel c) `elem` stmts) cs
      tryCatch (U.pi1 >>> run) (catchException run) -< (rest,clauses)
    Tableswitch i cases -> runSwitch run -< (i,cases)
    Lookupswitch i cases -> runSwitch run -< (i,cases)
    If e label -> first (evalBool &&& id) >>> if_ (atLabel run) run -< (e,(label,rest))
    Goto label -> (atLabel run) -< label
    Ret i -> liftAMaybe evalImmediate -< i
    Return i -> liftAMaybe evalImmediate -< i
    Throw i -> evalImmediate >>> failDynamic -< i
    Identity l i _ -> first (second evalAtIdentifier) >>> updateVar run -< ((l,i),rest)
    IdentityNoType l i -> first (second evalAtIdentifier) >>> updateVar run -< ((l,i),rest)
    Assign var e -> do
      v <- eval -< e
      case var of
        LocalVar l -> updateVar run -< ((l,v),rest)
        ReferenceVar ref -> case ref of
          ArrayRef l i -> first (first (readVar *** evalImmediate)) >>> updateIndex run -< (((l,i),v),rest)
          FieldRef l f -> first (first readVar) >>> updateField run -< ((l,(f,v)),rest)
          SignatureRef f -> do
            updateStaticField -< (f,v)
            run -< rest
    Invoke e -> do
      evalInvoke -< e
      run -< rest
    Nop -> run -< rest
    Breakpoint -> run -< rest
  where
    atLabel f = statementsFromLabel >>> f
    statementsFromLabel = proc label -> do
      (ss,_) <- ask -< ()
      case Label label `elemIndex` ss of
        Just i -> returnA -< drop i ss
        Nothing -> failStatic -< printf "Undefined label: %s" label
    catchException f = proc ((_,clauses),exception) ->
      catch (handleException f) -< (exception,clauses)
    handleException f = proc (val,clause) ->
      declare (atLabel f) -< (("@caughtexception",val),withLabel clause)
    runSwitch f = first evalImmediate >>> case_ (atLabel f)

runMethod :: CanInterp env envval val ex bool c => c (Maybe String,MethodSignature,[Immediate]) (Maybe val)
runMethod = proc (this,sig,params) -> do
  method <- askMethod -< sig
  (decs,stmts,clauses) <- askMethodBody -< method
  thisVal <- liftAMaybe readVar -< this
  case this of
    Just _ -> assert -< (Static `notElem` methodModifiers method,"Expected a non-static method for non-static invoke")
    Nothing -> assert -< (Static `elem` methodModifiers method,"Expected a static method for static invoke")
  let thisBinding = maybe [] (\x -> [("@this",x)]) thisVal
  paramVals <- U.map evalImmediate -< params
  let paramBindings = zip (map (\i -> "@parameter" ++ show i) [(0 :: Int)..]) paramVals
  decBindings <- U.map (second defaultValue) -< concatMap (\(t,d) -> zip d (repeat t)) decs
  env <- emptyEnv -< ()
  localEnv runWithBindings -< (env,(thisBinding ++ paramBindings ++ decBindings,(stmts,clauses)))
  where
    askMethod = proc sig@(MethodSignature c _ n _) -> do
      unit <- readCompilationUnit -< c
      case [m | MethodMember m <- fileBody unit, methodSignature unit m == sig] of
        m:_ -> returnA -< m
        [] -> failStatic -< printf "Method %s not defined for class %s" (show n) (show c)
    askMethodBody = proc m -> case methodBody m of
      EmptyBody -> failStatic -< "Cannot run method with empty body"
      FullBody{declarations=decs,statements=stmts,catchClauses=clauses} ->
        returnA -< (decs,stmts,clauses)
    runWithBindings = proc (bs,(stmts,clauses)) -> case bs of
      [] -> local runStatements -< ((stmts,clauses),stmts)
      (binding:rest) -> declare runWithBindings -< (binding,(rest,(stmts,clauses)))

runProgram :: CanInterp env envval val ex bool c => c (MethodSignature,[Immediate]) (Maybe val)
runProgram = proc (main,params) -> do
  units <- askCompilationUnits -< ()
  U.map (second defaultValue >>> updateStaticField) -< concatMap staticFieldsWithType units
  U.map runMethod -< concatMap clinitMethodWithArgs units
  runMethod -< (Nothing,main,params)
  where
    staticFieldsWithType u =
      [(fieldSignature u m,fieldType m) | FieldMember m <- fileBody u, Static `elem` fieldModifiers m]
    clinitMethodWithArgs u =
      [(Nothing,methodSignature u m,[]) | MethodMember m <- fileBody u, methodName m == "<clinit>"]

class Arrow c => UseVal v c | c -> v where
  doubleConstant :: c Float v
  floatConstant :: c Float v
  intConstant :: c Int v
  longConstant :: c Int v
  nullConstant :: c () v
  stringConstant :: c String v
  classConstant :: c String v
  newSimple :: c Type v
  newArray :: c (Type,[v]) v
  and :: c (v,v) v
  or :: c (v,v) v
  xor :: c (v,v) v
  rem :: c (v,v) v
  cmp :: c (v,v) v
  cmpg :: c (v,v) v
  cmpl :: c (v,v) v
  shl :: c (v,v) v
  shr :: c (v,v) v
  ushr :: c (v,v) v
  plus :: c (v,v) v
  minus :: c (v,v) v
  mult :: c (v,v) v
  div :: c (v,v) v
  lengthOf :: c v v
  neg :: c v v
  instanceOf :: c (v,Type) v
  cast :: c ((v,Type),v) v
  defaultValue :: c Type v
  declare :: c x (Maybe v) -> c ((String,v),x) (Maybe v)
  readVar :: c String v
  updateVar :: c x (Maybe v) -> c ((String,v),x) (Maybe v)
  readIndex :: c (v,v) v
  updateIndex :: c x (Maybe v) -> c (((v,v),v),x) (Maybe v)
  readField :: c (v,FieldSignature) v
  updateField :: c x (Maybe v) -> c ((v,(FieldSignature,v)),x) (Maybe v)
  readStaticField :: c FieldSignature v
  updateStaticField :: c (FieldSignature,v) ()
  case_ :: c String (Maybe v) -> c (v,[CaseStatement]) (Maybe v)

class (ArrowChoice c,ArrowFail (e v) c) => UseException e v c | c -> e v where
  failStatic :: c String a
  failDynamic :: c v a
  catch :: c (v,CatchClause) (Maybe v) -> c (e v,[CatchClause]) (Maybe v)

class Arrow c => UseBool b v c | c -> b v where
  eq :: c (v,v) b
  neq :: c (v,v) b
  gt :: c (v,v) b
  ge :: c (v,v) b
  lt :: c (v,v) b
  le :: c (v,v) b
  if_ :: c String (Maybe v) -> c x (Maybe v) -> c ((b,BoolExpr),(String,x)) (Maybe v)

class Arrow c => UseEnv env c | c -> env where
  emptyEnv :: c () env

class Arrow c => UseConst c where
  askCompilationUnits :: c () [CompilationUnit]
