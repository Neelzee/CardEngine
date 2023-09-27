module CDSL.CDSLExpr where


-- Expresions
data CDSLExpr = Var String CDSLType
  | CDSLFuncCall [CDSLExpr]


-- Name, Argument types, return type
data CDSLFuncCall = CDSLFuncCall String [CDSLType] CDSLType

-- Types
data CDSLType = CTInt
  | CTString
  | CTCard
  | CTPlayer


data CDSLValue = CVInt Int
  | CVString String
  | CVCard Card
  | CVPlayer Player