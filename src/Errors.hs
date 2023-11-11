module Errors where

import AbsLatte as A
import PrintLatte

semanticAnalysisError :: String
semanticAnalysisError = "SEMANTIC ANALYSIS ERROR "

showPosition :: BNFC'Position -> String
showPosition Nothing = "noPos "
showPosition (Just x) = show x ++ ": "

showPositionOf :: A.HasPosition a => a -> String
showPositionOf = showPosition . A.hasPosition

undefinedReferenceMessage :: A.Ident -> BNFC'Position -> String
undefinedReferenceMessage (Ident x) pos =
  showPosition pos ++ "undefined reference " ++ show x
