module Errors where

import Data.String (String)
import Grammar.AbsLatte (BNFC'Position)
import qualified Grammar.AbsLatte as A
import Grammar.PrintLatte

showPosition :: A.BNFC'Position -> String
showPosition Nothing = "noPos "
showPosition (Just x) = show x ++ ": "

showPositionOf :: A.HasPosition a => a -> String
showPositionOf = showPosition . A.hasPosition

undefinedReferenceMessage :: A.UIdent -> A.BNFC'Position -> String
undefinedReferenceMessage (A.UIdent x) pos =
  showPosition pos ++ "undefined reference " ++ show x

undefinedMember :: A.BNFC'Position -> A.UIdent -> A.UIdent -> String
undefinedMember pos c member = showPosition pos ++ "class " ++ printTree c ++ " doesn't have member " ++ printTree member

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

attributeAlreadyDeclaredForThisClassOrSuprclass :: A.BNFC'Position -> A.UIdent -> String
attributeAlreadyDeclaredForThisClassOrSuprclass pos ident =
  showPosition pos
    ++ "attribute with name "
    ++ printTree ident
    ++ " has already been declared for the superclass of this class"

functionDoesntReturnValue :: A.BNFC'Position -> String
functionDoesntReturnValue pos =
  showPosition pos
    ++ "function might not return a value"

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
    ++ " and different type has already been declared in its superclasses"

functionHasAlreadyBeenDeclared :: A.BNFC'Position -> A.UIdent -> String
functionHasAlreadyBeenDeclared pos ident =
  showPosition pos
    ++ "function with name "
    ++ printTree ident
    ++ "has already been declared"

classHasAlreadyBeenDeclared :: A.BNFC'Position -> A.UIdent -> String
classHasAlreadyBeenDeclared pos ident =
  showPosition pos
    ++ "class with name "
    ++ printTree ident
    ++ "has already been declared"

selfUsedOutsideOfClass :: BNFC'Position -> String
selfUsedOutsideOfClass pos = showPosition pos ++ "self used outside the class"

comparingValuesOfDifferentType :: BNFC'Position -> A.Type -> A.Type -> String
comparingValuesOfDifferentType pos t1 t2 =
  showPosition pos
    ++ "comparing values of diferent type, "
    ++ printTree t1
    ++ " and "
    ++ printTree t2

typeNotComparable :: BNFC'Position -> A.Type -> A.RelOp -> String
typeNotComparable pos t relop = showPosition pos ++ "type " ++ printTree t ++ " is not comparable with operation '" ++ printTree relop

noClassOfName :: A.BNFC'Position -> A.UIdent -> String
noClassOfName pos ident = showPosition pos ++ "no class '" ++ printTree ident ++ "'"

cyclicInheritance :: A.BNFC'Position -> String
cyclicInheritance pos = showPosition pos ++ "cyclic inheritance detected"

expectedTypeMessage :: A.Type -> String
expectedTypeMessage t = "expected type " ++ printTree t

unexpectedTypeMessage :: A.Type -> String
unexpectedTypeMessage t = "unexpected type " ++ printTree t
