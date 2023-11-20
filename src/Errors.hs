module Errors where

import Data.String (String)
import qualified Grammar.AbsLatte as A
import Grammar.PrintLatte

semanticAnalysisError :: String
semanticAnalysisError = "SEMANTIC ANALYSIS ERROR "

showPosition :: A.BNFC'Position -> String
showPosition Nothing = "noPos "
showPosition (Just x) = show x ++ ": "

showPositionOf :: A.HasPosition a => a -> String
showPositionOf = showPosition . A.hasPosition

undefinedReferenceMessage :: A.UIdent -> A.BNFC'Position -> String
undefinedReferenceMessage (A.UIdent x) pos =
  showPosition pos ++ "undefined reference " ++ show x

errorMessageWrongType :: A.BNFC'Position -> A.Type -> A.Type -> String
errorMessageWrongType pos received expected =
  showPosition pos
    ++ unexpectedTypeMessage received
    ++ ", "
    ++ expectedTypeMessage expected

errorMessageNotAnLValue :: A.BNFC'Position -> A.Expr -> String
errorMessageNotAnLValue pos expr =
  showPosition pos
    ++ "expression "
    ++ printTree expr
    ++ " is NOT an lvalue"

attributeAlreadyDeclaredForThisClass :: A.BNFC'Position -> A.UIdent -> String
attributeAlreadyDeclaredForThisClass pos ident =
  showPosition pos
    ++ "attribute with name "
    ++ printTree ident
    ++ " has already been declared in this class"

functionAlreadyDeclaredForThisClass :: A.BNFC'Position -> A.UIdent -> String
functionAlreadyDeclaredForThisClass pos ident =
  showPosition pos
    ++ "function with name "
    ++ printTree ident
    ++ " has already been declared in this class"

functionWithDifferentTypeAlreadyDeclaredForThisClass :: A.BNFC'Position -> A.UIdent -> String
functionWithDifferentTypeAlreadyDeclaredForThisClass pos ident =
  showPosition pos
    ++ "function with name "
    ++ printTree ident
    ++ " and different type has already been declared in this class or its superclasses"

functionHasAlreadyBeenDeclared :: A.BNFC'Position -> A.UIdent -> String
functionHasAlreadyBeenDeclared pos ident =
  showPosition pos
    ++ "function with name "
    ++ printTree ident
    ++ "has already been declared"

classHasAlreadyBeenDeclared :: A.BNFC'Position -> A.UIdent -> String
classHasAlreadyBeenDeclared pos ident =
  showPosition pos
    ++ "function with name "
    ++ printTree ident
    ++ "has already been declared"

cyclicInheritance :: A.BNFC'Position -> String
cyclicInheritance pos = showPosition pos ++ "cyclic inheritance detected"

expectedTypeMessage :: A.Type -> String
expectedTypeMessage t = "expected type " ++ printTree t

unexpectedTypeMessage :: A.Type -> String
unexpectedTypeMessage t = "unexpected type " ++ printTree t
