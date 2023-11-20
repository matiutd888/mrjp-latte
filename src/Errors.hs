module Errors where

import AbsLatte (BNFC'Position, HasPosition (hasPosition))
import AbsLatte as A
import qualified AbsLatte as A
import Data.String (String)
import PrintLatte

semanticAnalysisError :: String
semanticAnalysisError = "SEMANTIC ANALYSIS ERROR "

showPosition :: BNFC'Position -> String
showPosition Nothing = "noPos "
showPosition (Just x) = show x ++ ": "

showPositionOf :: A.HasPosition a => a -> String
showPositionOf = showPosition . A.hasPosition

undefinedReferenceMessage :: A.UIdent -> BNFC'Position -> String
undefinedReferenceMessage (UIdent x) pos =
  showPosition pos ++ "undefined reference " ++ show x

errorMessageWrongType :: BNFC'Position -> A.Type -> A.Type -> String
errorMessageWrongType pos received expected =
  showPosition pos
    ++ unexpectedTypeMessage received
    ++ ", "
    ++ (expectedTypeMessage expected)

errorMessageNotAnLValue :: BNFC'Position -> A.Expr -> String
errorMessageNotAnLValue pos expr =
  showPosition pos
    ++ "expression "
    ++ printTree expr
    ++ " is NOT an lvalue"

attributeAlreadyDeclaredForThisClass :: BNFC'Position -> A.UIdent -> String
attributeAlreadyDeclaredForThisClass pos ident =
  showPosition pos
    ++ "attribute with name "
    ++ printTree ident
    ++ " has already been declared in this class"

functionAlreadyDeclaredForThisClass :: BNFC'Position -> A.UIdent -> String
functionAlreadyDeclaredForThisClass pos ident =
  showPosition pos
    ++ "function with name "
    ++ printTree ident
    ++ " has already been declared in this class"

functionWithDifferentTypeAlreadyDeclaredForThisClass :: BNFC'Position -> A.UIdent -> String
functionWithDifferentTypeAlreadyDeclaredForThisClass pos ident =
  showPosition pos
    ++ "function with name "
    ++ printTree ident
    ++ " and different type has already been declared in this class or its superclasses"

functionHasAlreadyBeenDeclared :: BNFC'Position -> A.UIdent -> String
functionHasAlreadyBeenDeclared pos ident =
  showPosition pos
    ++ "function with name "
    ++ printTree ident
    ++ "has already been declared"

classHasAlreadyBeenDeclared :: BNFC'Position -> A.UIdent -> String
classHasAlreadyBeenDeclared pos ident =
  showPosition pos
    ++ "function with name "
    ++ printTree ident
    ++ "has already been declared"

cyclicInheritance :: BNFC'Position -> String
cyclicInheritance pos = showPosition pos ++ "cyclic inheritance detected"

expectedTypeMessage :: Type -> String
expectedTypeMessage t = "expected type " ++ printTree t

unexpectedTypeMessage :: Type -> String
unexpectedTypeMessage t = "unexpected type " ++ printTree t
