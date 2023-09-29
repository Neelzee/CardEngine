module CDSL.Expr where
import CardGame.Card (Card)
import CardGame.Player (Player)


-- Expresions
data CDSLExpr = Var String CDSLType
-- Name, Argument types, return type
  | CDSLFuncCall String [CDSLExpr] CDSLType

instance Eq CDSLExpr where
  (==) (Var s t) (Var s' t') = s == s' && t == t'
  (==) (CDSLFuncCall s ts t) (CDSLFuncCall s' ts' t') = s == s' && ts == ts' && t == t'
  (==) _ _ = False


-- Types
data CDSLType = CTInt
  | CTString
  | CTCard
  | CTPlayer
  deriving Eq


data CDSLValue = CVInt Int
  | CVString String
  | CVCard Card
  | CVPlayer Player

data CDSLFuncDecl = CDSLFuncDecl String [CDSLType] CDSLType

