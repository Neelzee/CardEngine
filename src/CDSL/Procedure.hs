module CDSL.Procedure where


import CDSL.Expr (CDSLExpr, CDSLValue, CDSLFuncDecl)



data CDSLProcedure = CDSLProcedure {
  args :: [CDSLExpr]
  , exprs :: [CDSLExpr]
}


type CDSLVariables = [(CDSLExpr, CDSLValue)]

type CDSLFunctions = [(CDSLFuncDecl, CDSLProcedure)]


data CDSLScope = CDSLScope {
  variables :: CDSLVariables
  , funcDecls :: CDSLFunctions
}