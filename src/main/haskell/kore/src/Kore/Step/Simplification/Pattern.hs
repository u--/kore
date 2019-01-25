{-|
Module      : Kore.Step.Simplification.Pattern
Description : Tools for Pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.Pattern
    ( simplify
    , simplifyToOr
    ) where

import           Kore.AST.MetaOrObject
import           Kore.AST.Pure
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools )
import           Kore.Step.ExpandedPattern
                 ( ExpandedPattern )
import           Kore.Step.Function.Data
                 ( BuiltinAndAxiomsFunctionEvaluatorMap )
import           Kore.Step.OrOfExpandedPattern
                 ( OrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( toExpandedPattern )
import           Kore.Step.Pattern
import qualified Kore.Step.Simplification.And as And
                 ( simplify )
import qualified Kore.Step.Simplification.Application as Application
                 ( simplify )
import qualified Kore.Step.Simplification.Bottom as Bottom
                 ( simplify )
import qualified Kore.Step.Simplification.Ceil as Ceil
                 ( simplify )
import qualified Kore.Step.Simplification.CharLiteral as CharLiteral
                 ( simplify )
import           Kore.Step.Simplification.Data
                 ( PredicateSubstitutionSimplifier, SimplificationProof (..),
                 Simplifier, StepPatternSimplifier (..) )
import qualified Kore.Step.Simplification.DomainValue as DomainValue
                 ( simplify )
import qualified Kore.Step.Simplification.Equals as Equals
                 ( simplify )
import qualified Kore.Step.Simplification.Exists as Exists
                 ( simplify )
import qualified Kore.Step.Simplification.Floor as Floor
                 ( simplify )
import qualified Kore.Step.Simplification.Forall as Forall
                 ( simplify )
import qualified Kore.Step.Simplification.Iff as Iff
                 ( simplify )
import qualified Kore.Step.Simplification.Implies as Implies
                 ( simplify )
import qualified Kore.Step.Simplification.In as In
                 ( simplify )
import qualified Kore.Step.Simplification.Next as Next
                 ( simplify )
import qualified Kore.Step.Simplification.Not as Not
                 ( simplify )
import qualified Kore.Step.Simplification.Or as Or
                 ( simplify )
import qualified Kore.Step.Simplification.Rewrites as Rewrites
                 ( simplify )
import qualified Kore.Step.Simplification.StringLiteral as StringLiteral
                 ( simplify )
import qualified Kore.Step.Simplification.Top as Top
                 ( simplify )
import qualified Kore.Step.Simplification.Variable as Variable
                 ( simplify )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )
import           Kore.Unparser
import           Kore.Variables.Fresh

import Debug.Trace

-- TODO(virgil): Add a Simplifiable class and make all pattern types
-- instances of that.

{-|'simplify' simplifies a StepPattern level variable, returning an
'ExpandedPattern'.
-}
simplify
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Show (variable level)
        , Ord (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level Simplifier
    -> BuiltinAndAxiomsFunctionEvaluatorMap level
    -- ^ Map from symbol IDs to defined functions
    -> StepPattern level variable
    -> Simplifier
        ( ExpandedPattern level variable
        , SimplificationProof level
        )
simplify tools substitutionSimplifier symbolIdToEvaluator patt = do
    (orPatt, proof) <-
        simplifyToOr tools symbolIdToEvaluator substitutionSimplifier patt
    return
        ( OrOfExpandedPattern.toExpandedPattern orPatt
        , proof
        )

{-|'simplifyToOr' simplifies a StepPattern level variable, returning an
'OrOfExpandedPattern'.
-}
simplifyToOr
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Show (variable level)
        , Ord (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        )
    => MetadataTools level StepperAttributes
    -> BuiltinAndAxiomsFunctionEvaluatorMap level
    -- ^ Map from symbol IDs to defined functions
    -> PredicateSubstitutionSimplifier level Simplifier
    -> StepPattern level variable
    -> Simplifier
        ( OrOfExpandedPattern level variable
        , SimplificationProof level
        )
simplifyToOr tools symbolIdToEvaluator substitutionSimplifier patt = trace "###simplifyToOr" $ 
    simplifyInternal
        tools
        substitutionSimplifier
        simplifier
        symbolIdToEvaluator
        (fromPurePattern patt)
  where
    simplifier = StepPatternSimplifier
        (simplifyToOr tools symbolIdToEvaluator)

simplifyInternal
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Show (variable level)
        , Ord (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level Simplifier
    -> StepPatternSimplifier level variable
    -> BuiltinAndAxiomsFunctionEvaluatorMap level
    -- ^ Map from symbol IDs to defined functions
    -> Base (StepPattern level variable) (StepPattern level variable)
    -> Simplifier
        ( OrOfExpandedPattern level variable
        , SimplificationProof level
        )
simplifyInternal
    tools
    substitutionSimplifier
    simplifier@(StepPatternSimplifier unwrappedSimplifier)
    symbolIdToEvaluator
    (valid :< patt)
  = do
    halfSimplified <- traverse (unwrappedSimplifier substitutionSimplifier) patt
    -- TODO: Remove fst
    case fmap fst halfSimplified of
        AndPattern p -> And.simplify tools substitutionSimplifier p
        ApplicationPattern p ->
            --  TODO: Re-evaluate outside of the application and stop passing
            -- the simplifier.
            Application.simplify
                tools
                substitutionSimplifier
                simplifier
                symbolIdToEvaluator
                (valid :< p)
        BottomPattern p -> return $ Bottom.simplify p
        CeilPattern p -> return $ Ceil.simplify tools p
        DomainValuePattern p -> return $ DomainValue.simplify tools p
        EqualsPattern p -> Equals.simplify tools substitutionSimplifier p
        ExistsPattern p ->
            Exists.simplify tools substitutionSimplifier simplifier p
        FloorPattern p -> return $ Floor.simplify p
        ForallPattern p -> return $ Forall.simplify p
        IffPattern p -> return $ Iff.simplify p
        ImpliesPattern p -> return $ Implies.simplify p
        InPattern p -> return $ In.simplify tools p
        -- TODO(virgil): Move next up through patterns.
        NextPattern p -> return $ Next.simplify p
        NotPattern p -> return $ Not.simplify p
        OrPattern p -> return $ Or.simplify p
        RewritesPattern p -> return $ Rewrites.simplify p
        StringLiteralPattern p -> return $ StringLiteral.simplify p
        CharLiteralPattern p -> return $ CharLiteral.simplify p
        TopPattern p -> return $ Top.simplify p
        VariablePattern p -> return $ Variable.simplify p
