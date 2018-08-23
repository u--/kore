module Test.Kore.Builtin.Builtin where

import Test.QuickCheck
       ( Property, (===) )

import           Data.Map
                 ( Map )
import qualified Data.Map as Map
import           Data.Proxy
                 ( Proxy (..) )

import           Kore.AST.Common
import           Kore.AST.MetaOrObject
                 ( Object )
import           Kore.AST.PureML
                 ( CommonPurePattern )
import           Kore.AST.Sentence
import           Kore.ASTUtils.SmartPatterns
import           Kore.ASTVerifier.DefinitionVerifier
import           Kore.ASTVerifier.Error
                 ( VerifyError )
import qualified Kore.Builtin as Builtin
import qualified Kore.Builtin.Bool as Bool
import           Kore.Builtin.Hook
                 ( hookAttribute )
import qualified Kore.Error
import           Kore.IndexedModule.IndexedModule
import           Kore.IndexedModule.MetadataTools
import           Kore.Step.ExpandedPattern
import           Kore.Step.Simplification.Data
import qualified Kore.Step.Simplification.Pattern as Pattern
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )

import Test.Kore
       ( testId )

-- | Make an unparameterized builtin symbol with the given name.
builtinSymbol :: String -> SymbolOrAlias Object
builtinSymbol name =
    SymbolOrAlias
        { symbolOrAliasConstructor = testId name
        , symbolOrAliasParams = []
        }

-- | Declare 'boolSort' in a Kore module.
hookedSortDecl 
     :: Sort Object
     -> String
     -> KoreSentence
hookedSortDecl sort hookName =
    (asSentence . SentenceHookedSort) (SentenceSort
        { sentenceSortName =
            let SortActualSort SortActual { sortActualName } = sort
            in sortActualName
        , sentenceSortParameters = []
        , sentenceSortAttributes = Attributes [ hookAttribute hookName ]
        }
        :: KoreSentenceSort Object)

-- | Declare a symbol hooked to the given builtin name.
hookedSymbolDecl
    :: String
    -- ^ builtin name
    -> SymbolOrAlias Object
    -- ^ symbol
    -> Sort Object
    -- ^ result sort
    -> [Sort Object]
    -- ^ argument sorts
    -> KoreSentence
hookedSymbolDecl
    builtinName
    SymbolOrAlias { symbolOrAliasConstructor }
    sentenceSymbolResultSort
    sentenceSymbolSorts
  =
    (asSentence . SentenceHookedSymbol)
        (SentenceSymbol
            { sentenceSymbolSymbol
            , sentenceSymbolSorts
            , sentenceSymbolResultSort
            , sentenceSymbolAttributes
            }
            :: KoreSentenceSymbol Object
        )
  where
    sentenceSymbolSymbol =
        Symbol
            { symbolConstructor = symbolOrAliasConstructor
            , symbolParams = []
            }
    sentenceSymbolAttributes = Attributes [ hookAttribute builtinName ]

verify
    :: Builtin.Verifiers
    -> KoreDefinition
    -> Either (Kore.Error.Error VerifyError)
        (Map ModuleName (KoreIndexedModule StepperAttributes))
verify builtinVerifiers = verifyAndIndexDefinition attrVerify builtinVerifiers
  where
    attrVerify = defaultAttributesVerification Proxy

evaluate :: KoreIndexedModule StepperAttributes -> CommonPurePattern Object -> CommonPurePattern Object
evaluate indexedModule pat =
    case evalSimplifier (Pattern.simplify tools builtinFunctions pat) of
        Left err -> error (Kore.Error.printError err)
        Right (ExpandedPattern { term }, _) -> term
  where
    tools = extractMetadataTools indexedModule
    builtinFunctions = Builtin.functionContext Bool.builtinFunctions indexedModule