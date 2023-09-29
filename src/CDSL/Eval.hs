module CDSL.Eval where

import CDSL.Expr (CDSLExpr (..), CDSLValue)
import CDSL.Error (CDSLError (CDSLError))
import CDSL.Procedure (CDSLScope (..))

evalExpr :: CDSLExpr -> CDSLScope -> Either CDSLValue CDSLError
evalExpr = undefined



