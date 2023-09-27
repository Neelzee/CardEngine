module CDSL.CDSLScope where



data CDSLScope = CDSLScopre {
  variables :: [(CDSLExpr, CDSLValue)]
  , functionDeclerations :: [(CDSLFuncCall, CDSLExpr)]
}