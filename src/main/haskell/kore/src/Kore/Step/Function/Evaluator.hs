{-|
Module      : Kore.Step.Function.Evaluator
Description : Evaluates functions in a pattern.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Function.Evaluator
    ( evaluateApplication
    ) where

import           Control.Exception
                 ( assert )
import           Control.Monad
                 ( when )
import qualified Data.Map as Map
import           Data.Maybe
                 ( isJust )
import           Data.Reflection
                 ( give )
import qualified Data.Text as Text
import           Data.These
                 ( these )

import           Kore.AST.Pure
import           Kore.AST.Valid
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..) )
import           Kore.Predicate.Predicate
                 ( makeTruePredicate )
import           Kore.Step.AxiomPatterns
                 ( EqualityRule (EqualityRule) )
import           Kore.Step.BaseStep
                 ( OrStepResult (OrStepResult),
                 UnificationProcedure (UnificationProcedure),
                 stepWithRemaindersForUnifier )
import qualified Kore.Step.BaseStep as OrStepResult
                 ( OrStepResult (..) )
import           Kore.Step.ExpandedPattern
                 ( ExpandedPattern, PredicateSubstitution, Predicated (..) )
import           Kore.Step.Function.Data
                 ( ApplicationFunctionEvaluator (..),
                 BuiltinAndAxiomsFunctionEvaluatorMap,
                 FunctionEvaluators (FunctionEvaluators) )
import           Kore.Step.Function.Data as FunctionEvaluators
                 ( FunctionEvaluators (..) )
import           Kore.Step.Function.Data as AttemptedFunction
                 ( AttemptedFunction (..) )
import           Kore.Step.Function.Matcher
                 ( unificationWithAppMatchOnTop )
import qualified Kore.Step.Merging.OrOfExpandedPattern as OrOfExpandedPattern
                 ( mergeWithPredicateSubstitution )
import           Kore.Step.OrOfExpandedPattern
                 ( OrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( extractPatterns, make, traverseWithPairs )
import           Kore.Step.Pattern
import           Kore.Step.Simplification.Data
                 ( PredicateSubstitutionSimplifier, SimplificationProof (..),
                 Simplifier, StepPatternSimplifier (..) )
import qualified Kore.Step.Simplification.ExpandedPattern as ExpandedPattern
import           Kore.Step.StepperAttributes
                 ( Hook (..), StepperAttributes (..), isSortInjection_ )
import           Kore.Unparser
import           Kore.Variables.Fresh

import Debug.Trace

{-| 'evaluateApplication' - evaluates functions on an application pattern.
-}
evaluateApplication
    ::  forall level variable.
        ( MetaOrObject level
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        , SortedVariable variable
        )
    => MetadataTools level StepperAttributes
    -- ^ Tools for finding additional information about patterns
    -- such as their sorts, whether they are constructors or hooked.
    -> PredicateSubstitutionSimplifier level Simplifier
    -> StepPatternSimplifier level variable
    -- ^ Evaluates functions.
    -> BuiltinAndAxiomsFunctionEvaluatorMap level
    -- ^ Map from symbol IDs to defined functions
    -> PredicateSubstitution level variable
    -- ^ Aggregated children predicate and substitution.
    -> CofreeF
        (Application level)
        (Valid (variable level) level)
        (StepPattern level variable)
    -- ^ The pattern to be evaluated
    -> Simplifier
        (OrOfExpandedPattern level variable, SimplificationProof level)
evaluateApplication
    tools
    substitutionSimplifier
    simplifier
    symbolIdToEvaluator
    childrenPredicateSubstitution
    validApp@(valid :< app)
  = trace "###S.Func.Eval.evalApp" $ 
    case Map.lookup symbolId symbolIdToEvaluator of
        Nothing
          | give tools isSortInjection_ appHead ->
            evaluateSortInjection tools unchangedOr app
          | Just hook <- getAppHookString
          , not(null symbolIdToEvaluator) ->
            error
                (   "Attempting to evaluate unimplemented hooked operation "
                ++  hook ++ ".\nSymbol: " ++ show (getId symbolId)
                )
          | otherwise -> trace "###otherwiseReturnUnchanged" $ 
            return unchanged
        Just builtinOrAxiomEvaluators -> trace "###jsutBuiltinOrAxiomEvaluators" $ 
            these
                evaluateWithBuiltins
                evaluateWithFunctionAxioms
                evaluateBuiltinAndAxioms
                builtinOrAxiomEvaluators
  where
    Application { applicationSymbolOrAlias = appHead } = app
    SymbolOrAlias { symbolOrAliasConstructor = symbolId } = appHead

    appPurePattern = asPurePattern (valid :< ApplicationPattern app)

    evaluateBuiltinAndAxioms
        :: ApplicationFunctionEvaluator level
        -> FunctionEvaluators level
        -> Simplifier
            (OrOfExpandedPattern level variable, SimplificationProof level)
    evaluateBuiltinAndAxioms builtinEvaluator axiomEvaluators = trace "###evalBuiltinAndAxioms" $ do
        (result, _proof) <- applyEvaluator validApp builtinEvaluator
        case result of
            AttemptedFunction.NotApplicable
              | isAppConcrete
              , Just hook <- getAppHookString ->
                error
                    (   "Expecting hook " ++ hook
                    ++  " to reduce concrete pattern\n\t"
                    ++ show app
                    )
              | otherwise ->
                -- If the builtin axioms failed, in many cases we can't just
                -- apply evaluation axioms, since may of them are recursive
                -- and will be applied indefinitely.
                -- TODO(virgil): We should refine this at some point.
                evaluateWithFunctionAxioms
                    axiomEvaluators { definitionRules = [] }
            AttemptedFunction.Applied pat -> return (pat, SimplificationProof)

    unchangedPatt =
        case childrenPredicateSubstitution of
            Predicated { predicate, substitution } ->
                Predicated
                    { term         = appPurePattern
                    , predicate    = predicate
                    , substitution = substitution
                    }
    unchangedOr = OrOfExpandedPattern.make [unchangedPatt]
    unchanged = (unchangedOr, SimplificationProof)

    applyEvaluator
        :: CofreeF
            (Application level)
            (Valid (variable level) level)
            (StepPattern level variable)
        -> ApplicationFunctionEvaluator level
        -> Simplifier
            ( AttemptedFunction level variable
            , SimplificationProof level
            )
    applyEvaluator
        app' (ApplicationFunctionEvaluator evaluator)
      = do
        result <- evaluator
            tools
            substitutionSimplifier
            simplifier
            app'
        mergeWithConditionAndSubstitution
            tools
            substitutionSimplifier
            simplifier
            childrenPredicateSubstitution
            result

    isAppConcrete = isJust (asConcretePurePattern appPurePattern)
    getAppHookString =
        Text.unpack <$> (getHook . hook . symAttributes tools) appHead

    evaluateWithFunctionAxioms
        :: FunctionEvaluators level
        -> Simplifier
            (OrOfExpandedPattern level variable, SimplificationProof level)
    evaluateWithFunctionAxioms
        FunctionEvaluators { definitionRules, simplificationEvaluators }
      = do
        (simplifiedResult, proof) <-
            evaluateWithSimplificationAxioms simplificationEvaluators
        case simplifiedResult of
            AttemptedFunction.NotApplicable ->
                if null definitionRules
                    then
                        -- We don't have a definition, so we shouldn't attempt
                        -- to evaluate it, since it would currently evaluate
                        -- to bottom.
                        return (OrOfExpandedPattern.make [unchangedPatt], proof)
                    else evaluateWithDefinitionAxioms definitionRules
            AttemptedFunction.Applied result -> return (result, proof)

    evaluateWithSimplificationAxioms [] =
        return
            ( AttemptedFunction.NotApplicable
            , SimplificationProof
            )
    evaluateWithSimplificationAxioms (evaluator : evaluators) = trace "###evaluateWithSimplificationAxioms" $ do
        (applicationResult, _proof) <- applyEvaluator validApp evaluator

        let
            simplify
                :: ExpandedPattern level variable
                -> Simplifier [ExpandedPattern level variable]
            simplify result = trace "###simplify" $ do
                orPatt <- reevaluateFunctions
                    tools
                    substitutionSimplifier
                    simplifier
                    result
                return (OrOfExpandedPattern.extractPatterns orPatt)
        case applicationResult of
            AttemptedFunction.Applied orResults -> trace "###orResults" $ do
                when
                    (length (OrOfExpandedPattern.extractPatterns orResults) > 1)
                    -- We should only allow multiple simplification results
                    -- when they are created by unification splitting the
                    -- configuration.
                    -- However, right now, we shouldn't be able to get more
                    -- than one result, so we throw an error.
                    (error
                        (  "Unexpected simplification result with more "
                        ++ "than one configuration: "
                        ++ show orResults
                        )
                    )
                patts <-
                    mapM
                        simplify
                        (OrOfExpandedPattern.extractPatterns orResults)
                return
                    ( AttemptedFunction.Applied
                        (OrOfExpandedPattern.make (concat patts))
                    , SimplificationProof
                    )
            AttemptedFunction.NotApplicable -> trace "###NotApplicable" $ 
                evaluateWithSimplificationAxioms evaluators

    evaluateWithBuiltins evaluator = do
        (result, _proof) <- applyEvaluator validApp evaluator
        case result of
            AttemptedFunction.NotApplicable -> return
                ( OrOfExpandedPattern.make [unchangedPatt]
                , SimplificationProof
                )
            AttemptedFunction.Applied orResult -> do
                -- TODO(virgil): Find out if builtin results need to be
                -- resimplified and, if they don't, skip resimplification.
                simplifiedPatts <-
                    mapM simplifyIfNeeded
                        (OrOfExpandedPattern.extractPatterns orResult)
                return
                    ( OrOfExpandedPattern.make (concat simplifiedPatts)
                    , SimplificationProof
                    )

    evaluateWithDefinitionAxioms
        :: [EqualityRule level]
        -> Simplifier
            (OrOfExpandedPattern level variable, SimplificationProof level)
    evaluateWithDefinitionAxioms definitionRules = do
        (OrStepResult { rewrittenPattern, remainder }, _proof) <-
            stepWithRemaindersForUnifier
                tools
                (UnificationProcedure unificationWithAppMatchOnTop)
                substitutionSimplifier
                (map (\ (EqualityRule rule) -> rule) definitionRules)
                unchangedPatt
        let
            evaluationResults :: [ExpandedPattern level variable]
            evaluationResults =
                OrOfExpandedPattern.extractPatterns rewrittenPattern

            remainderResults :: [ExpandedPattern level variable]
            remainderResults =
                    OrOfExpandedPattern.extractPatterns remainder

            simplifyPredicate
                :: ExpandedPattern level variable
                -> Simplifier
                    (ExpandedPattern level variable, SimplificationProof level)
            simplifyPredicate =
                ExpandedPattern.simplifyPredicate
                    tools
                    substitutionSimplifier
                    simplifier
        simplifiedEvaluationLists <- mapM simplifyIfNeeded evaluationResults
        simplifiedRemainderList <-
            mapM simplifyPredicate remainderResults
        let
            simplifiedEvaluationResults :: [ExpandedPattern level variable]
            simplifiedEvaluationResults = concat simplifiedEvaluationLists

            simplifiedRemainderResults :: [ExpandedPattern level variable]
            (simplifiedRemainderResults, _proofs) =
                unzip simplifiedRemainderList
        return
            ( OrOfExpandedPattern.make
                (simplifiedEvaluationResults ++ simplifiedRemainderResults)
            , SimplificationProof
            )

    simplifyIfNeeded
        :: ExpandedPattern level variable
        -> Simplifier [ExpandedPattern level variable]
    simplifyIfNeeded result = trace "###simplifyIfNeeded" $ 
        if result == unchangedPatt
            then return [unchangedPatt]
            else do
                orPatt <- reevaluateFunctions
                    tools
                    substitutionSimplifier
                    simplifier
                    result
                return (OrOfExpandedPattern.extractPatterns orPatt)

{-| 'reevaluateFunctions' re-evaluates functions after a user-defined function
was evaluated.
-}
reevaluateFunctions
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        )
    => MetadataTools level StepperAttributes
    -- ^ Tools for finding additional information about patterns
    -- such as their sorts, whether they are constructors or hooked.
    -> PredicateSubstitutionSimplifier level Simplifier
    -> StepPatternSimplifier level variable
    -- ^ Evaluates functions in patterns.
    -> ExpandedPattern level variable
    -- ^ Function evaluation result.
    -> Simplifier (OrOfExpandedPattern level variable)
reevaluateFunctions
    tools
    substitutionSimplifier
    wrappedSimplifier@(StepPatternSimplifier simplifier)
    Predicated
        { term   = rewrittenPattern
        , predicate = rewritingCondition
        , substitution = rewrittenSubstitution
        }
  = trace "###Step.Func.Eval.reEvaluateFunctions" $ do
    (pattOr , _proof) <-
        simplifier substitutionSimplifier rewrittenPattern
    (mergedPatt, _proof) <-
        OrOfExpandedPattern.mergeWithPredicateSubstitution
            tools
            substitutionSimplifier
            wrappedSimplifier
            Predicated
                { term = ()
                , predicate = rewritingCondition
                , substitution = rewrittenSubstitution
                }
            pattOr
    (evaluatedPatt, _) <-
        OrOfExpandedPattern.traverseWithPairs
            (ExpandedPattern.simplifyPredicate
                tools substitutionSimplifier wrappedSimplifier
            )
            mergedPatt
    return evaluatedPatt

evaluateSortInjection
    :: (MetaOrObject level, Ord (variable level))
    => MetadataTools level StepperAttributes
    -> OrOfExpandedPattern level variable
    -> Application level (StepPattern level variable)
    -> Simplifier (OrOfExpandedPattern level variable, SimplificationProof level)
evaluateSortInjection tools unchanged ap = case apChild of
    (App_ apHeadChild grandChildren)
      | give tools isSortInjection_ apHeadChild ->
        let
            [fromSort', toSort'] = symbolOrAliasParams apHeadChild
            apHeadNew = updateSortInjectionSource apHead fromSort'
        in
            assert (toSort' == fromSort) $
            return
                ( OrOfExpandedPattern.make
                    [ Predicated
                        { term = apHeadNew grandChildren
                        , predicate = makeTruePredicate
                        , substitution = mempty
                        }
                    ]
                , SimplificationProof
                )
    _ -> return (unchanged, SimplificationProof)
  where
    apHead = applicationSymbolOrAlias ap
    [fromSort, _] = symbolOrAliasParams apHead
    [apChild] = applicationChildren ap
    updateSortInjectionSource head1 fromSort1 =
        mkApp toSort1 head1 { symbolOrAliasParams = [fromSort1, toSort1] }
      where
        [_, toSort1] = symbolOrAliasParams head1

{-| 'mergeWithCondition' ands the given condition-substitution to the given
function evaluation.
-}
mergeWithConditionAndSubstitution
    ::  ( MetaOrObject level
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        , SortedVariable variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level Simplifier
    -> StepPatternSimplifier level variable
    -- ^ Evaluates functions in a pattern.
    -> PredicateSubstitution level variable
    -- ^ Condition and substitution to add.
    -> (AttemptedFunction level variable, SimplificationProof level)
    -- ^ AttemptedFunction to which the condition should be added.
    -> Simplifier (AttemptedFunction level variable, SimplificationProof level)
mergeWithConditionAndSubstitution
    _ _ _ _ (AttemptedFunction.NotApplicable, _proof)
  =
    return (AttemptedFunction.NotApplicable, SimplificationProof)
mergeWithConditionAndSubstitution
    tools
    substitutionSimplifier
    simplifier
    toMerge
    (AttemptedFunction.Applied functionResult, _proof)
  = do
    (evaluated, _proof) <- OrOfExpandedPattern.mergeWithPredicateSubstitution
        tools
        substitutionSimplifier
        simplifier
        toMerge
        functionResult
    return (AttemptedFunction.Applied evaluated, SimplificationProof)
