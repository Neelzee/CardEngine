module CDSL.Exec where
import CDSL.Procedure (CDSLProcedure (..), CDSLScope (..), CDSLVariables, CDSLFunctions)
import CDSL.Error (CDSLError (CDSLError))

import Data.Maybe (mapMaybe)
import CDSL.Expr (CDSLExpr (..), CDSLValue, CDSLFuncDecl (CDSLFuncDecl))



execProc :: CDSLScope -> CDSLProcedure -> Either CDSLScope CDSLError
execProc (CDSLScope vars funDecls) p = case verifyArgs (args p) vars of
    Left args -> undefined
    Right e -> Right e



verifyArgs :: [CDSLExpr] -> CDSLVariables -> Either [CDSLValue] CDSLError
verifyArgs xs ys = case mapMaybe (`lookup` ys) xs of
    [] -> if null xs then
            Left []
        else
            Right CDSLError
    zs -> Left zs


verifyFunDecl :: CDSLExpr -> CDSLScope -> Either CDSLProcedure CDSLError
verifyFunDecl _ (CDSLScope { funcDecls = [] }) = Right CDSLError
verifyFunDecl fc@(CDSLFuncCall fn args rt) sc@(CDSLScope { funcDecls = ((CDSLFuncDecl fn' ts' rt', p):fs) })
    | fn /= fn' = verifyFunDecl fc (sc {funcDecls = fs})
    | rt /= rt' = verifyFunDecl fc (sc {funcDecls = fs})
    | otherwise = case verifyArgs args (variables sc) of
      Left vls -> undefined
      Right e -> Right e
        
verifyFunDecl _ _ = Right CDSLError