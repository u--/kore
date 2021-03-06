{-|
Module      : Kore.Step.Function.Matcher
Description : Matches free-form patterns which can be used when applying
              Equals rules.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Function.Matcher
    ( matchAsUnification
    ) where

import           Control.Applicative
                 ( (<|>) )
import           Control.Error.Util
                 ( just, nothing )
import           Control.Monad.Counter
                 ( MonadCounter )
import           Control.Monad.Except
import           Control.Monad.Trans.Except
                 ( ExceptT (..) )
import           Control.Monad.Trans.Maybe
                 ( MaybeT (..) )
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Kore.AST.Pure
import           Kore.AST.Valid
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools )
import           Kore.Step.ExpandedPattern
                 ( PredicateSubstitution, Predicated (..) )
import qualified Kore.Step.ExpandedPattern as Predicated
import           Kore.Step.OrOfExpandedPattern
                 ( MultiOr, OrOfPredicateSubstitution )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( filterOr, fullCrossProduct, make )
import           Kore.Step.Pattern
import           Kore.Step.RecursiveAttributes
                 ( isFunctionPattern )
import qualified Kore.Step.Simplification.Ceil as Ceil
                 ( makeEvaluateTerm )
import           Kore.Step.Simplification.Data
                 ( PredicateSubstitutionSimplifier )
import qualified Kore.Step.Simplification.Equals as Equals
                 ( makeEvaluateTermsToPredicateSubstitution )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )
import           Kore.Step.Substitution
                 ( mergePredicatesAndSubstitutionsExcept )
import           Kore.Unification.Error
                 ( UnificationError (..), UnificationOrSubstitutionError (..) )
import qualified Kore.Unification.Substitution as Substitution
import           Kore.Unification.Unifier
                 ( UnificationProof (..) )
import           Kore.Unparser
import           Kore.Variables.Free
                 ( freePureVariables )
import           Kore.Variables.Fresh
                 ( FreshVariable )

{- Matches two patterns based on their form.

Assumes that the two patterns have no common variables (quantified or not).

Returns Right bottom or Left when it can't handle the patterns. The
returned substitution substitutes only variables from the first pattern.

The meaning of a Right value is that the substitution holds IF the predicate
holds.

TODO: This is different from unification's meaning, so we should either
convert all bottoms to Left, or we should do it selectively. Doing
it selectively is not trivial, e.g. a bottom inside a function should become
Left, but inside a constructor we may be able to keep it as bottom.
-}
matchAsUnification
    ::  ( FreshVariable variable
        , MetaOrObject level
        , Ord (variable level)
        , Ord (variable Object)
        , Ord (variable Meta)
        , Show (variable level)
        , Show (variable Object)
        , Show (variable Meta)
        , Unparse (variable level)
        , SortedVariable variable
        , MonadCounter m
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level m
    -> StepPattern level variable
    -> StepPattern level variable
    -> ExceptT
        (UnificationOrSubstitutionError level variable)
        m
        ( OrOfPredicateSubstitution level variable
        , UnificationProof level variable
        )
matchAsUnification tools substitutionSimplifier first second = do
    result <- runMaybeT matchResult
    case result of
        Nothing -> throwError (UnificationError UnsupportedPatterns)
        Just r -> return (r, EmptyUnificationProof)
  where
    matchResult = match tools substitutionSimplifier Map.empty first second

match
    ::  ( FreshVariable variable
        , MetaOrObject level
        , Ord (variable level)
        , Ord (variable Object)
        , Ord (variable Meta)
        , Show (variable level)
        , Show (variable Object)
        , Show (variable Meta)
        , Unparse (variable level)
        , SortedVariable variable
        , MonadCounter m
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level m
    -> Map.Map (variable level) (variable level)
    -> StepPattern level variable
    -> StepPattern level variable
    -- TODO: Use Result here.
    -> MaybeT
        (ExceptT
            (UnificationOrSubstitutionError level variable)
            m
        )
        (OrOfPredicateSubstitution level variable)
match tools substitutionSimplifier quantifiedVariables first second =
    matchEqualHeadPatterns
        tools substitutionSimplifier quantifiedVariables first second
    <|> matchVariableFunction tools quantifiedVariables first second
    <|> matchNonVarToPattern tools substitutionSimplifier first second

matchEqualHeadPatterns
    ::  forall level variable m.
        ( Show (variable level)
        , SortedVariable variable
        , MetaOrObject level
        , Ord (variable level)
        , Unparse (variable level)
        , Ord (variable Meta)
        , Ord (variable Object)
        , Show (variable Meta)
        , Show (variable Object)
        , FreshVariable variable
        , MonadCounter m
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level m
    -> Map.Map (variable level) (variable level)
    -> StepPattern level variable
    -> StepPattern level variable
    -> MaybeT
        (ExceptT
            (UnificationOrSubstitutionError level variable)
            m
        )
        (OrOfPredicateSubstitution level variable)
matchEqualHeadPatterns
    tools substitutionSimplifier quantifiedVariables first second
  =
    case first of
        (And_ _ firstFirst firstSecond) ->
            case second of
                (And_ _ secondFirst secondSecond) ->
                    matchJoin
                        tools
                        substitutionSimplifier
                        quantifiedVariables
                        [ (firstFirst, secondFirst)
                        , (firstSecond, secondSecond)
                        ]
                _ -> nothing
        (App_ firstHead firstChildren) ->
            case second of
                (App_ secondHead secondChildren) ->
                    if firstHead == secondHead
                    then
                        matchJoin
                            tools
                            substitutionSimplifier
                            quantifiedVariables
                            (zip firstChildren secondChildren)
                    else nothing
                _ -> nothing
        (Bottom_ _) -> topWhenEqualOrNothing first second
        (Ceil_ _ _ firstChild) ->
            case second of
                (Ceil_ _ _ secondChild) ->
                    match
                        tools
                        substitutionSimplifier
                        quantifiedVariables
                        firstChild
                        secondChild
                _ -> nothing
        (CharLiteral_ _) ->
            topWhenEqualOrNothing first second
        (DV_ _ _) ->
            topWhenEqualOrNothing first second
        (Equals_ _ _ firstFirst firstSecond) ->
            case second of
                (Equals_ _ _ secondFirst secondSecond) ->
                    matchJoin
                        tools
                        substitutionSimplifier
                        quantifiedVariables
                        [ (firstFirst, secondFirst)
                        , (firstSecond, secondSecond)
                        ]
                _ -> nothing
        (Exists_ _ firstVariable firstChild) ->
            case second of
                (Exists_ _ secondVariable secondChild) ->
                    checkVariableEscapeOr [firstVariable, secondVariable]
                    <$> match
                        tools
                        substitutionSimplifier
                        (Map.insert
                            firstVariable secondVariable quantifiedVariables
                        )
                        firstChild
                        secondChild
                _ -> nothing
        (Floor_ _ _ firstChild) ->
            case second of
                (Floor_ _ _ secondChild) ->
                    match
                        tools
                        substitutionSimplifier
                        quantifiedVariables
                        firstChild
                        secondChild
                _ -> nothing
        (Forall_ _ firstVariable firstChild) ->
            case second of
                (Forall_ _ secondVariable secondChild) ->
                    (<$>)
                        (checkVariableEscapeOr [firstVariable, secondVariable])
                        (match
                            tools
                            substitutionSimplifier
                            (Map.insert
                                firstVariable
                                secondVariable
                                quantifiedVariables
                            )
                            firstChild
                            secondChild
                        )
                _ -> nothing
        (Iff_ _ firstFirst firstSecond) ->
            case second of
                (Iff_ _ secondFirst secondSecond) ->
                    matchJoin
                        tools
                        substitutionSimplifier
                        quantifiedVariables
                        [ (firstFirst, secondFirst)
                        , (firstSecond, secondSecond)
                        ]
                _ -> nothing
        (Implies_ _ firstFirst firstSecond) ->
            case second of
                (Implies_ _ secondFirst secondSecond) ->
                    matchJoin
                        tools
                        substitutionSimplifier
                        quantifiedVariables
                        [ (firstFirst, secondFirst)
                        , (firstSecond, secondSecond)
                        ]
                _ -> nothing
        (In_ _ _ firstFirst firstSecond) ->
            case second of
                (In_ _ _ secondFirst secondSecond) ->
                    matchJoin
                        tools
                        substitutionSimplifier
                        quantifiedVariables
                        [ (firstFirst, secondFirst)
                        , (firstSecond, secondSecond)
                        ]
                _ -> nothing
        (Next_ _ firstChild) ->
            case second of
                (Next_ _ secondChild) ->
                    match
                        tools
                        substitutionSimplifier
                        quantifiedVariables
                        firstChild
                        secondChild
                _ -> nothing
        (Not_ _ firstChild) ->
            case second of
                (Not_ _ secondChild) ->
                    match
                        tools
                        substitutionSimplifier
                        quantifiedVariables
                        firstChild
                        secondChild
                _ -> nothing
        (Or_ _ firstFirst firstSecond) ->
            case second of
                (Or_ _ secondFirst secondSecond) ->
                    matchJoin
                        tools
                        substitutionSimplifier
                        quantifiedVariables
                        [ (firstFirst, secondFirst)
                        , (firstSecond, secondSecond)
                        ]
                _ -> nothing
        (Rewrites_ _ firstFirst firstSecond) ->
            case second of
                (Rewrites_ _ secondFirst secondSecond) ->
                    matchJoin
                        tools
                        substitutionSimplifier
                        quantifiedVariables
                        [ (firstFirst, secondFirst)
                        , (firstSecond, secondSecond)
                        ]
                _ -> nothing
        (StringLiteral_ _) -> topWhenEqualOrNothing first second
        (Top_ _) -> topWhenEqualOrNothing first second
        (Var_ firstVariable) ->
            case second of
                (Var_ secondVariable) ->
                    case Map.lookup firstVariable quantifiedVariables of
                        Nothing -> nothing
                        Just variable ->
                            if variable == secondVariable
                            then justTop
                            else nothing
                _ -> nothing
        _ -> nothing
  where
    topWhenEqualOrNothing first' second' =
        if first' == second'
            then justTop
            else nothing
    justTop
        :: MaybeT
            (ExceptT
                (UnificationOrSubstitutionError level variable)
                m
            )
            (OrOfPredicateSubstitution level variable)
    justTop = just
        (OrOfExpandedPattern.make [Predicated.topPredicate])

matchJoin
    :: forall level variable m .
        ( FreshVariable variable
        , MetaOrObject level
        , Ord (variable level)
        , Ord (variable Meta)
        , Ord (variable Object)
        , Show (variable level)
        , Show (variable Object)
        , Show (variable Meta)
        , Unparse (variable level)
        , SortedVariable variable
        , MonadCounter m
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level m
    -> Map.Map (variable level) (variable level)
    -> [(StepPattern level variable, StepPattern level variable)]
    -> MaybeT
        (ExceptT
            (UnificationOrSubstitutionError level variable)
            m
        )
        (OrOfPredicateSubstitution level variable)
matchJoin tools substitutionSimplifier quantifiedVariables patterns
  = do
    matched <-
        traverse
            (uncurry $ match tools substitutionSimplifier quantifiedVariables)
            patterns
    let
        crossProduct :: MultiOr [PredicateSubstitution level variable]
        crossProduct = OrOfExpandedPattern.fullCrossProduct matched
        merge
            :: [PredicateSubstitution level variable]
            -> ExceptT
                (UnificationOrSubstitutionError level variable)
                m
                (PredicateSubstitution level variable)
        merge items = do
            (result, _proof) <- mergePredicatesAndSubstitutionsExcept
                tools
                substitutionSimplifier
                (map Predicated.predicate items)
                (map Predicated.substitution items)
            return result
    OrOfExpandedPattern.filterOr <$> traverse (lift . merge) crossProduct

-- Note that we can't match variables to stuff which can have more than one
-- value, because if we take the axiom
-- x = x and exists y . y=x
-- and we try to apply it to, say, 'a or b', where a and b are constructors
-- without arguments, then we would get
-- a or b
--   = (a or b) and (exists y . y = (a or b))
--   = (a or b) and bottom
--   = bottom
--
-- However, we can match variables to non-total stuff by using ceil to
-- force the match to bottom whenever we lose totality. This
-- assumes that, when applying the match to a pattern p, it will be split
-- into (p-replacing-lhs-by-rhs[subst] and predicate) or (p and not predicate)
matchVariableFunction
    :: ( Show (variable level)
       , SortedVariable variable
       , MetaOrObject level
       , Ord (variable level)
       , Unparse (variable level)
       , MonadCounter m
       )
    => MetadataTools level StepperAttributes
    -> Map.Map (variable level) (variable level)
    -> StepPattern level variable
    -> StepPattern level variable
    -> MaybeT m (OrOfPredicateSubstitution level variable)
matchVariableFunction
    tools
    quantifiedVariables
    (Var_ var)
    second
  | not (var `Map.member` quantifiedVariables)
    && isFunctionPattern tools second
  = case Ceil.makeEvaluateTerm tools second of
        (predicate, _proof) ->
            just $
                OrOfExpandedPattern.make
                    [ Predicated
                        { term = ()
                        , predicate
                        , substitution = Substitution.wrap [(var, second)]
                        }
                    ]
matchVariableFunction _ _ _ _ = nothing

matchNonVarToPattern
    :: forall level variable m .
        ( FreshVariable variable
        , MetaOrObject level
        , Ord (variable level)
        , Ord (variable Object)
        , Ord (variable Meta)
        , Show (variable level)
        , Show (variable Object)
        , Show (variable Meta)
        , Unparse (variable level)
        , SortedVariable variable
        , MonadCounter m
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level m
    -> StepPattern level variable
    -> StepPattern level variable
    -> MaybeT
        (ExceptT
            (UnificationOrSubstitutionError level variable)
            m
        )
        (OrOfPredicateSubstitution level variable)
matchNonVarToPattern tools substitutionSimplifier first second =
    -- TODO(virgil): For simplification axioms this would need to return bottom!
    MaybeT $ lift $ do -- MonadCounter
        (result, _proof) <-
            Equals.makeEvaluateTermsToPredicateSubstitution
                tools substitutionSimplifier first second
        (return . return)
            (fmap secondVariablesSubstitutionToPredicate result)
  where
    secondVariablesSubstitutionToPredicate
        :: PredicateSubstitution level variable
        -> PredicateSubstitution level variable
    secondVariablesSubstitutionToPredicate
        Predicated {term = (), predicate, substitution}
      = Predicated
        { term = ()
        , predicate = finalPredicate
        , substitution = Substitution.wrap leftSubst
        }
      where
        leftVars = freePureVariables first
        rawSubstitution = Substitution.unwrap substitution
        (leftSubst, rightSubst) =
            List.partition ((`elem` leftVars) . fst) rawSubstitution
        finalPredicate =
            Predicated.toPredicate Predicated
                { term = ()
                , predicate
                , substitution = Substitution.wrap rightSubst
                }

checkVariableEscapeOr
    ::  ( MetaOrObject level
        , Show (variable Object)
        , Show (variable Meta)
        , Ord (variable Object)
        , Ord (variable Meta)
        , SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        )
    => [variable level]
    -> OrOfPredicateSubstitution level variable
    -> OrOfPredicateSubstitution level variable
checkVariableEscapeOr vars = fmap (checkVariableEscape vars)

checkVariableEscape
    ::  ( MetaOrObject level
        , Show (variable Object)
        , Show (variable Meta)
        , Ord (variable Object)
        , Ord (variable Meta)
        , SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        )
    => [variable level]
    -> PredicateSubstitution level variable
    -> PredicateSubstitution level variable
checkVariableEscape vars predSubst
  | any (`Set.member` freeVars) vars = error
        "quantified variables in substitution or predicate escaping context"
  | otherwise = predSubst
  where
    freeVars = Predicated.freeVariables predSubst
