module Test.Kore.Builtin.Map where

-- import Test.QuickCheck
--        ( Property, (===) )

import qualified Data.Map
-- import qualified Data.Map as Map

import           Kore.AST.Common
import           Kore.AST.MetaOrObject
                 ( Object )
import           Kore.AST.PureML
                 ( CommonPurePattern )
import           Kore.AST.Sentence
import           Kore.ASTUtils.SmartPatterns
-- import           Kore.ASTUtils.SmartConstructors
-- import           Kore.ASTVerifier.DefinitionVerifier
-- import           Kore.ASTVerifier.Error
                 -- ( VerifyError )
import qualified Kore.Builtin as Builtin
import qualified Kore.Builtin.Map as Map
-- import           Kore.Builtin.Hook
                 -- ( hookAttribute )
-- import qualified Kore.Error
import           Kore.IndexedModule.IndexedModule
-- import           Kore.IndexedModule.MetadataTools
-- import           Kore.Step.ExpandedPattern
-- import           Kore.Step.Simplification.Data
-- import qualified Kore.Step.Simplification.Pattern as Pattern
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )
import           Test.Kore.Builtin.Builtin

import Test.Kore
       ( testId )

-- import Debug.Trace
-- import Kore.Proof.Dummy

-- | A sort to hook to the builtin @MAP.Map@.
-- Should actually be parametric
mapSort :: Sort Object
mapSort =
    SortActualSort SortActual
        { sortActualName = testId "Map"
        , sortActualSorts = []
        }

-- | Declare 'boolSort' in a Kore module.
mapSortDecl :: KoreSentence
mapSortDecl = hookedSortDecl mapSort "MAP.Map"

symElement :: SymbolOrAlias Object
symElement = builtinSymbol "|->" 

symElementDecl :: KoreSentence
symElementDecl = hookedSymbolDecl "MAP.element" symElement mapSort [mapSort, mapSort]

symLookup :: SymbolOrAlias Object
symLookup = builtinSymbol "lookup"

symLookupDecl :: KoreSentence
symLookupDecl = hookedSymbolDecl "MAP.lookup" symLookup mapSort [mapSort, mapSort]

mapModuleName :: ModuleName
mapModuleName = ModuleName "MAP"

mapModule :: KoreModule
mapModule =
    Module
        { moduleName = mapModuleName
        , moduleAttributes = Attributes []
        , moduleSentences = 
            [ mapSortDecl
            , symElementDecl
            , symLookupDecl
            ]
        }

mapDefinition :: KoreDefinition
mapDefinition =
    Definition
        { definitionAttributes = Attributes []
        , definitionModules = [ mapModule ]
        }

mapBuiltinVerifiers :: Builtin.Verifiers
mapBuiltinVerifiers =
    Builtin.Verifiers
        { sortDeclVerifiers = Map.sortDeclVerifiers
        , symbolVerifiers = Map.symbolVerifiers
        , patternVerifier = Map.patternVerifier
        }

indexedModules :: Data.Map.Map ModuleName (KoreIndexedModule StepperAttributes)
Right indexedModules = verify mapBuiltinVerifiers mapDefinition

indexedModule :: KoreIndexedModule StepperAttributes
Just indexedModule = Data.Map.lookup mapModuleName indexedModules


lookupFromElem 
    :: CommonPurePattern Object 
    -> CommonPurePattern Object
    -> CommonPurePattern Object
    -> CommonPurePattern Object
lookupFromElem k1 k2 v = App_ symLookup [k1, App_ symElement [k2, v]]

-- tryLookupFromElem k1 k2 v = evaluate indexedModule (lookupFromElem k1 k2 v)

-- pat1 = dummyEnvironment $ mkVar (var_ "KEY" "*") :: CommonPurePattern Object
-- pat2 = dummyEnvironment $ mkVar (var_ "VALUE" "*") :: CommonPurePattern Object

