module Compile where

import qualified Data.Map as M
import qualified Grammar.AbsLatte as A

type Loc = Integer

data Env = Env
  { variables :: M.Map A.UIdent Loc, -- Location relative to rbp
    variableTypes :: M.Map A.UIdent A.Type
  }
