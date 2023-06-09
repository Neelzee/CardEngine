module CDSL.CDSLValidater (
    validateCDSLExpression
    , isCardField
    , isComperator
    , isCard
    , getNonCards
    , getNonComperators
    , getNonCardFields
    , getNonNumerics
    , getAllCards
    ) where
import CDSL.CDSLExpr
    ( CDSLExecError(..),
      CDSLExecErrorCode(InvalidBoolEvaluationError,
                        UnknownExpressionError, SyntaxErrorLeftOperand,
                        SyntaxErrorRightOperand, InvalidSyntaxError),
      CDSLExpr(CEffect, Any, All, Greatest, Shuffle, Swap, Take, Reset,
               If, Text, Numeric, PlayerAction, TORight, TOLeft, Put,
               GoBack, GoForward, Turn, CEq, CLEq, CGRq, CLe, CGr, Look, Pile,
               Deck, Discard, Always, Never, IsEqual, Not, Or, And, IsEmpty,
               IsSame, CurrentPlayer, PreviousPlayer, IsMove, PAPass, CardSuit,
               CardRank, CardValue, PMoves, Hand, Players, Score, Cards) )
import Data.Either (partitionEithers, isLeft)
import CDSL.ParseCDSLExpr (isCDSLExprNumeric)
import Feature ()




-- Checks if a given CDSLExpr is valid.
-- Due to this language being more command-like
-- , there are only a certain ways expressions can be built up.
-- But I have to manually add them here, since this is a "whitelist" of valid expressions.
validateCDSLExpression :: CDSLExpr -> Either CDSLExpr [CDSLExecError]
-- Players
validateCDSLExpression e@(Any ex) = if isLeft $ validateCDSLBool ex
    then
        Left e
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e})]
validateCDSLExpression e@(All ex) = if isLeft $ validateCDSLBool ex
    then
        Left e
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e})]
validateCDSLExpression e@(Greatest lst) = if isNumericList lst
    then
        Left e
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e })]
-- Cards
validateCDSLExpression e@(Shuffle a) = if isList a
    then
        Left e
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e })]
validateCDSLExpression e@(IsEmpty a) = if isList a
    then
        Left e
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e })]
validateCDSLExpression e@(Swap a b) = if isList a && isList b
    then
        Left e
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e})]
validateCDSLExpression e@(Take n f t) = case (isCDSLExprNumeric n, isList f, isList t, isSameList f t) of
    (True, True, True, True) -> Left e
    _ -> Right [(CDSLExecError { err = InvalidSyntaxError, expr = e})]
validateCDSLExpression (And l r) = case (partitionEithers $ map validateCDSLBool l, partitionEithers $ map validateCDSLBool r) of
    ((_, []), (_, [])) -> Left (And l r)
    ((_, er), _) -> Right (CDSLExecError { err = SyntaxErrorLeftOperand, expr = And l r }:er)
validateCDSLExpression (Or l r) = case (partitionEithers $ map validateCDSLBool l, partitionEithers $ map validateCDSLBool r) of
    ((_, []), (_, [])) -> Left (Or l r)
    ((_, er), _) -> Right (CDSLExecError { err = SyntaxErrorLeftOperand, expr = Or l r }:er)
validateCDSLExpression Always = Left Always
validateCDSLExpression Never = Left Never
validateCDSLExpression ex@(Reset (CurrentPlayer e)) = if isPlayerField e
    then
        Left ex
    else
        Right [(CDSLExecError { err = InvalidSyntaxError, expr = e})]
validateCDSLExpression e@(If conds exprs) = case (partitionEithers (map validateCDSLBool conds), partitionEithers (map validateCDSLExpression exprs)) of
    ((_, []), (_, [])) -> Left e
    ((_, cErrs), (_, eErrs)) -> Right (CDSLExecError { err = InvalidBoolEvaluationError, expr = e }:cErrs ++ concat eErrs)
validateCDSLExpression t@(Text _) = Left t
validateCDSLExpression n@(Numeric _) = Left n
validateCDSLExpression CardSuit = Left CardSuit
validateCDSLExpression CardRank = Left CardRank
validateCDSLExpression CardValue = Left CardValue
validateCDSLExpression pa@(PlayerAction _ _) = Left pa
validateCDSLExpression ce@(CEffect _ _) = Left ce
validateCDSLExpression TORight = Left TORight
validateCDSLExpression TOLeft = Left TOLeft
validateCDSLExpression CEq = Left CEq
validateCDSLExpression CLe = Left CLe
validateCDSLExpression CGr = Left CGr
validateCDSLExpression CLEq = Left CLEq
validateCDSLExpression CGRq = Left CGRq
validateCDSLExpression c@(Cards _) = Left c
validateCDSLExpression p@(Put a b) = case (isList a, isList b) of
    (True, True) -> Left p
    (True, False) -> Right [CDSLExecError { err = SyntaxErrorRightOperand, expr = Put a b }]
    (False, True) -> Right [CDSLExecError { err = SyntaxErrorLeftOperand, expr = Put a b }]
    _ -> Right [CDSLExecError { err = InvalidSyntaxError, expr = Put a b }]
validateCDSLExpression e@(GoBack Turn) = Left e
validateCDSLExpression e@(GoForward Turn) = Left e
validateCDSLExpression e = Right [CDSLExecError { err = UnknownExpressionError, expr = e }]

isComperator :: CDSLExpr -> Bool
isComperator CEq = True
isComperator CLEq = True
isComperator CGRq = True
isComperator CLe = True
isComperator CGr = True
isComperator _ = False

-- Checks if the given expression is a list
isList :: CDSLExpr -> Bool
isList (Look n lst) = isCDSLExprNumeric n && isList lst
isList Pile = True
isList Deck = True
isList Discard = True
isList (Players ex) = isList ex
isList (CurrentPlayer ex) = isList ex
isList Hand = True
isList Score = True
isList _ = False


-- Checks if the given expressions are lists of the same type
isSameList :: CDSLExpr -> CDSLExpr -> Bool
isSameList (Look _ lst) a = isSameList lst a
isSameList (Players ex) a = isSameList ex a
isSameList a (Look _ lst) = isSameList a lst
isSameList a (Players ex) = isSameList a ex
isSameList Pile Deck = True
isSameList Pile Discard = True
isSameList Deck Pile = True
isSameList Deck Discard = True
isSameList Discard Deck = True
isSameList Discard Pile = True
isSameList a b = a == Hand && (b == Pile || b == Deck || b == Discard)
    || b == Hand && (a == Pile || a == Deck || a == Discard)


validateCDSLBool :: CDSLExpr -> Either CDSLExpr CDSLExecError
validateCDSLBool Always = Left Always
validateCDSLBool Never = Left Never
validateCDSLBool e@(IsEqual l r) = case (isCDSLExprNumeric l, isCDSLExprNumeric r) of
    (True, True) -> Left e
    (False, True) -> Right (CDSLExecError { err = SyntaxErrorLeftOperand, expr = e })
    (True, False) -> Right (CDSLExecError { err = SyntaxErrorRightOperand, expr = e })
    _ -> Right (CDSLExecError { err = InvalidSyntaxError, expr = e })
validateCDSLBool e@(Not ex) = case partitionEithers (map validateCDSLBool ex) of
    (_, []) -> Left e
    (_, err) -> Right (head err)
validateCDSLBool (Or l r) = case (partitionEithers $ map validateCDSLBool l, partitionEithers $ map validateCDSLBool r) of
    ((lt, []), (rt, [])) -> Left (Or lt rt)
    ((_, err), _) -> Right (head err)
validateCDSLBool (And l r) = case (partitionEithers $ map validateCDSLBool l, partitionEithers $ map validateCDSLBool r) of
    ((lt, []), (rt, [])) -> Left (And lt rt)
    ((_, err), _) -> Right (head err)
validateCDSLBool e@(IsEmpty a) = if isList a
    then
        Left e
    else
        Right (CDSLExecError { err = InvalidSyntaxError, expr = e })
validateCDSLBool is@(IsSame cf lst) = if isList lst && isCardField cf
    then
        Left is
    else
        Right (CDSLExecError { err = InvalidBoolEvaluationError, expr = is })
validateCDSLBool exs@(Players ex) = case validateCDSLBool ex of
    Left _ -> Left exs
    Right e -> Right (e { expr = exs })
validateCDSLBool exs@(CurrentPlayer (IsMove PAPass)) = Left exs
validateCDSLBool exs@(PreviousPlayer (IsMove PAPass)) = Left exs
validateCDSLBool e = Right (CDSLExecError { err = InvalidBoolEvaluationError, expr = e })


-- Checks if the given expression is a card field
isCardField :: CDSLExpr -> Bool
isCardField CardSuit = True
isCardField CardRank = True
isCardField CardValue = True
isCardField _ = False

-- Checks if the given expression is a player field
isPlayerField :: CDSLExpr -> Bool
isPlayerField PMoves = True
isPlayerField Hand = True
isPlayerField Score = True
isPlayerField _ = False

-- Checks if the given expression is a numeric list
isNumericList :: CDSLExpr -> Bool
isNumericList (Players Score) = True
isNumericList _ = False




isCard :: CDSLExpr -> Bool
isCard (Cards _) = True
isCard (CEffect _ _) = True
isCard _ = False



getNonCards :: CDSLExpr -> Maybe CDSLExpr
getNonCards x
    | isCard x = Nothing
    | otherwise = Just x


getAllCards :: CDSLExpr -> Maybe CDSLExpr
getAllCards x
    | isCard x = Just x
    | otherwise = Nothing

getNonComperators :: CDSLExpr -> Maybe CDSLExpr
getNonComperators x
    | isComperator x = Nothing
    | otherwise = Just x


getNonCardFields :: CDSLExpr -> Maybe CDSLExpr
getNonCardFields x
    | isCardField x = Nothing
    | otherwise = Just x


getNonNumerics :: CDSLExpr -> Maybe CDSLExpr
getNonNumerics x
    | isCDSLExprNumeric x = Nothing
    | otherwise = Just x