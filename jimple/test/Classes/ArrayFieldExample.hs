module Classes.ArrayFieldExample where

import Syntax

import Java.Lang.Object

arrayFieldExampleArrSignature :: FieldSignature
arrayFieldExampleArrSignature = FieldSignature
  "ArrayFieldExample"
  (ArrayType IntType)
  "arr"

arrayFieldExampleArrField :: Field
arrayFieldExampleArrField = Field {
  fieldModifiers = [Public],
  fieldType = ArrayType IntType,
  fieldName = "arr"
}

arrayFieldExampleInitSignature :: MethodSignature
arrayFieldExampleInitSignature = MethodSignature
  "ArrayFieldExample"
  VoidType
  "<init>"
  []

arrayFieldExampleInitMethod :: Method
arrayFieldExampleInitMethod = Method {
  methodModifiers = [Public],
  returnType = VoidType,
  methodName = "<init>",
  parameters = [],
  throws = [],
  methodBody = FullBody {
    declarations = [
      (RefType "ArrayFieldExample", ["r0"])
    ],
    statements = [
      Identity "r0" ThisRef (RefType "ArrayFieldExample"),
      Invoke (SpecialInvoke "r0" objectInitSignature []),
      Return Nothing
    ],
    catchClauses = []
  }
}

arrayFieldExampleMainMethod :: Method
arrayFieldExampleMainMethod = Method {
  methodModifiers = [Public, Static],
  returnType = VoidType,
  methodName = "main",
  parameters = [
    IntType
  ],
  throws = [],
  methodBody = FullBody {
    declarations = [
      (IntType, ["r0"]),
      (ArrayType IntType, ["r1"]),
      (RefType "ArrayFieldExample", ["$r2"])
    ],
    statements = [
      Identity "r0" (ParameterRef 0) IntType,
      Assign (LocalVar "$r2") (NewExpr (RefType "ArrayFieldExample")),
      Invoke (SpecialInvoke "$r2" arrayFieldExampleInitSignature []),
      Invoke (VirtualInvoke "$r2" arrayFieldExampleFillSignature [Local "r0"]),
      Assign (LocalVar "r1") (RefExpr (FieldRef "$r2" arrayFieldExampleArrSignature)),
      Return (Just (Local "r1"))
    ],
    catchClauses = []
  }
}

arrayFieldExampleFillSignature :: MethodSignature
arrayFieldExampleFillSignature = MethodSignature
  "ArrayFieldExample"
  VoidType
  "fill"
  [IntType]

arrayFieldExampleFillMethod :: Method
arrayFieldExampleFillMethod = Method {
  methodModifiers = [Private],
  returnType = VoidType,
  methodName = "fill",
  parameters = [IntType],
  throws = [],
  methodBody = FullBody {
    declarations = [
      (RefType "ArrayFieldExample", ["r0"]),
      (IntType, ["p0","p1"]),
      (ArrayType IntType, ["a0"])
    ],
    statements = [
      Identity "r0" ThisRef (RefType "ArrayFieldExample"),
      Assign (LocalVar "a0") (NewArrayExpr IntType (IntConstant 4)),
      Identity "p0" (ParameterRef 0) IntType,
      Assign (ReferenceVar (ArrayRef "a0" (IntConstant 0))) (ImmediateExpr (Local "p0")),
      Assign (LocalVar "p0") (BinopExpr (Local "p0") Mult (IntConstant 2)),
      Assign (ReferenceVar (ArrayRef "a0" (IntConstant 1))) (ImmediateExpr (Local "p0")),
      Identity "p1" (ParameterRef 0) IntType,
      Assign (ReferenceVar (ArrayRef "a0" (IntConstant 2))) (ImmediateExpr (Local "p1")),
      Assign (LocalVar "p1") (BinopExpr (Local "p1") Mult (IntConstant 2)),
      Assign (ReferenceVar (ArrayRef "a0" (IntConstant 3))) (ImmediateExpr (Local "p1")),
      Assign (ReferenceVar (FieldRef "r0" arrayFieldExampleArrSignature)) (ImmediateExpr (Local "a0")),
      Return Nothing
    ],
    catchClauses = []
  }
}

arrayFieldExampleFile :: CompilationUnit
arrayFieldExampleFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = ClassFile,
  fileName = "ArrayFieldExample",
  extends = Just "java.lang.Object",
  implements = [],
  fileBody = [
    FieldMember arrayFieldExampleArrField,
    MethodMember arrayFieldExampleInitMethod,
    MethodMember arrayFieldExampleMainMethod,
    MethodMember arrayFieldExampleFillMethod
  ]
}
