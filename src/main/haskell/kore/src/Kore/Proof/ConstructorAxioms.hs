{-|
Module      : Kore.Proof.ConstructorAxioms
Description : No-junk, No-confusion etc. for non-AC constructors
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : phillip.harris@runtimeverification.com
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

{-# OPTIONS_GHC -Wno-unused-matches    #-}
{-# OPTIONS_GHC -Wno-name-shadowing    #-}


module Kore.Proof.ConstructorAxioms
( generateInjectivityAxiom
, generateNoConfusionAxiom
) where

import Data.Reflection
import Kore.AST.Common
import Kore.AST.MetaOrObject
import Kore.IndexedModule.MetadataTools

import Kore.ASTUtils.SmartConstructors

import Kore.Proof.Proof
import Kore.Proof.Util



-- | Given head symbol h, return sort of h, arguments sorts s_i,
-- generates axiom of the form:
-- forall x_1 ... x_n , forall y_1 ... y_n
-- h(x_1, ..., x_n) = h(y_1, ..., y_n) ->
-- x_1 = y_1 /\ x_2 = y_2 /\ ... /\ x_n = y_n
-- where x_i, y_i : s_i
generateInjectivityAxiom
    :: Given (MetadataTools Object)
    => SymbolOrAlias Object
    -> Sort Object
    -> [Sort Object]
    -> Term
generateInjectivityAxiom head resultSort childrenSorts =
    let vars name =
            zipWith
                (\n sort -> varS (name ++ show n) sort)
                [(1::Int)..]
                childrenSorts
        xVars = vars "x"
        xVars' = map Var_ xVars
        yVars = vars "y"
        yVars' = map Var_ yVars
        fxEqfy =
            mkApp head xVars'
            `mkEquals`
            mkApp head yVars'
        xsEqys = mkAndN $ zipWith mkEquals xVars' yVars'
    in mkForallN xVars $ mkForallN yVars $ (fxEqfy `mkImplies` xsEqys)

-- | No confusion: two different constructors cannot generate the same term.
-- `not (f(x_1,...,x_n) = g(y_1,...,y_m))`
generateNoConfusionAxiom
    :: Given (MetadataTools Object)
    => SymbolOrAlias Object
    -> [Sort Object]
    -> SymbolOrAlias Object
    -> [Sort Object]
    -> Term
generateNoConfusionAxiom h1 c1 h2 c2 =
    let (_, xVars') = generateVarList c1 "x"
        (_, yVars') = generateVarList c2 "y"
    in mkNot $ mkEquals
         (mkApp h1 xVars')
         (mkApp h2 yVars')

-- generateCoveringAxiom cons =
--     mkOrN $ map