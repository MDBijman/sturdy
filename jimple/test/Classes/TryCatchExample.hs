module Classes.TryCatchExample where

import Syntax

import Java.Lang.ArrayIndexOutOfBoundsException

tryCatchExampleMainMethod :: Method
tryCatchExampleMainMethod = Method {
  methodModifiers = [Public, Static],
  returnType = VoidType,
  methodName = "main",
  parameters = [],
  throws = [],
  methodBody = FullBody {
    declarations = [
      (RefType "java.lang.ArrayIndexOutOfBoundsException", ["$r2", "$r3", "$r4"])
    ],
    statements = [
      Label "label1",
      Assign (LocalVar "$r2") (NewExpr (RefType "java.lang.ArrayIndexOutOfBoundsException")),
      Invoke (SpecialInvoke "$r2" arrayIndexOutOfBoundsExceptionInitSignature [StringConstant "a"]),
      Throw (Local "$r2"),
      Label "label2",
      IdentityNoType "$r3" CaughtExceptionRef,
      Assign (LocalVar "$r4") (NewExpr (RefType "java.lang.ArrayIndexOutOfBoundsException")),
      Invoke (SpecialInvoke "$r4" arrayIndexOutOfBoundsExceptionInitSignature [StringConstant "b"]),
      Throw (Local "$r4")
    ],
    catchClauses = [
      CatchClause { className = "java.lang.ArrayIndexOutOfBoundsException"
                  , fromLabel = "label1"
                  , toLabel   = "label2"
                  , withLabel = "label2"
                  }
    ]
  }
}

tryCatchExampleFile :: CompilationUnit
tryCatchExampleFile = CompilationUnit {
  fileModifiers = [Public],
  fileType = ClassFile,
  fileName = "TryCatchExample",
  extends = Just "java.lang.Object",
  implements = [],
  fileBody = [
    MethodMember tryCatchExampleMainMethod
  ]
}
