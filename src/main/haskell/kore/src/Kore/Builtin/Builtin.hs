{- |
Module      : Kore.Builtin.Builtin
Description : Built-in sort, symbol, and pattern verifiers
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
Stability   : experimental
Portability : portable

This module is intended to be imported qualified, to avoid collision with other
builtin modules.

@
    import qualified Kore.Builtin.Builtin as Builtin
@
 -}

{-# OPTIONS_GHC -Wno-unused-matches #-}

module Kore.Builtin.Builtin
    (
      -- * Using builtin verifiers
      Verifiers (..)
    , SymbolVerifier, SymbolVerifiers
    , SortDeclVerifier, SortDeclVerifiers
    , SortVerifier
    , PatternVerifier (..)
    , Function
    , Parser
    , symbolVerifier
    , sortDeclVerifier
      -- * Declaring builtin verifiers
    , verifySortDecl
    , verifySort
    , verifySymbol
    , verifySymbolArguments
    , verifyDomainValue
    , verifyStringLiteral
    , parseDomainValue
    , notImplemented
    , binaryOperator
    , unaryOperator
    ) where

import           Control.Monad
                 ( zipWithM_ )
import           Control.Monad.Except
                 ( MonadError )
import qualified Control.Monad.Except as Except
import qualified Data.Functor.Foldable as Functor.Foldable
import           Data.HashMap.Strict
                 ( HashMap )
import qualified Data.HashMap.Strict as HashMap
import           Data.Semigroup
                 ( Semigroup (..) )
import           Data.Void
                 ( Void )
import           Text.Megaparsec
                 ( Parsec )
import qualified Text.Megaparsec as Parsec

import           Kore.AST.Common
                 ( Application (..), DomainValue (..), Id (..),
                 Pattern (DomainValuePattern, StringLiteralPattern), Sort (..),
                 SortActual (..), SortVariable (..), StringLiteral (..),
                 Symbol (..), SymbolOrAlias, Variable )
import           Kore.AST.Error
                 ( withLocationAndContext )
import           Kore.AST.Kore
                 ( CommonKorePattern )
import           Kore.AST.MetaOrObject
                 ( Meta, Object )
import           Kore.AST.PureML
                 ( CommonPurePattern )
import           Kore.AST.Sentence
                 ( KoreSentenceSort, KoreSentenceSymbol, SentenceSort (..),
                 SentenceSymbol (..) )
import           Kore.ASTHelpers
                 ( ApplicationSorts (..) )
import           Kore.ASTUtils.SmartPatterns
                 ( pattern StringLiteral_ )
import           Kore.ASTVerifier.Error
                 ( VerifyError )
import           Kore.Attribute.Parser
                 ( parseAttributes )
import           Kore.Builtin.Hook
                 ( Hook (..) )
import           Kore.Error
                 ( Error )
import qualified Kore.Error
import           Kore.IndexedModule.IndexedModule
                 ( SortDescription )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..) )
import qualified Kore.Predicate.Predicate as Predicate
import           Kore.Step.ExpandedPattern
                 ( ExpandedPattern (..) )
import           Kore.Step.Function.Data
                 ( ApplicationFunctionEvaluator (ApplicationFunctionEvaluator),
                 AttemptedFunction (..) )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
import           Kore.Step.Simplification.Data
                 ( SimplificationProof (..) )

type Parser = Parsec Void String

type Function = ApplicationFunctionEvaluator Object Variable

-- | Verify a sort declaration.
type SortDeclVerifier =
       KoreSentenceSort Object
    -- ^ Sort declaration to verify
    -> Either (Error VerifyError) ()

type SortVerifier =
       (Id Object -> Either (Error VerifyError) (SortDescription Object))
    -- ^ Find a sort declaration
    -> Sort Object
    -- ^ Sort to verify
    -> Either (Error VerifyError) ()

-- | @SortDeclVerifiers@ associates a @SortDeclVerifier@ with its builtin sort name.
type SortDeclVerifiers = HashMap String SortDeclVerifier

type SymbolVerifier =
       (Id Object -> Either (Error VerifyError) (SortDescription Object))
    -- ^ Find a sort declaration
    -> KoreSentenceSymbol Object
    -- ^ Symbol declaration to verify
    -> Either (Error VerifyError) ()

{- | @SymbolVerifiers@ associates a @SymbolVerifier@ with each builtin
  symbol name.
 -}
type SymbolVerifiers = HashMap String SymbolVerifier

newtype PatternVerifier =
    PatternVerifier
    {
      {- | Verify object-level patterns in builtin sorts.

        The first argument is a function to look up sorts.
        The second argument is a (projected) object-level pattern to verify.
        If the pattern is not in a builtin sort, the verifier should
        @return ()@.

        See also: 'verifyDomainValue'
      -}
      runPatternVerifier
          :: (Id Object -> Either (Error VerifyError) (SortDescription Object))
          -> Pattern Object Variable CommonKorePattern
          -> Either (Error VerifyError) ()
    }

instance Semigroup PatternVerifier where
    {- | Conjunction of 'PatternVerifier's.

      The resulting @PatternVerifier@ succeeds when both constituents succeed.

     -}
    (<>) a b =
        PatternVerifier
        { runPatternVerifier = \findSort pat -> do
            runPatternVerifier a findSort pat
            runPatternVerifier b findSort pat
        }

instance Monoid PatternVerifier where
    {- | Trivial 'PatternVerifier' (always succeeds).
     -}
    mempty = PatternVerifier { runPatternVerifier = \_ _ -> return () }
    mappend = (<>)

type DomainValueVerifier =
    DomainValue Object (CommonPurePattern Meta) -> Either (Error VerifyError) ()

{- | Verify builtin sorts, symbols, and patterns.
 -}
data Verifiers =
    Verifiers
    { sortDeclVerifiers :: SortDeclVerifiers
    , symbolVerifiers :: SymbolVerifiers
    , patternVerifier :: PatternVerifier
    }

{- | Look up and apply a builtin sort declaration verifier.

  The 'Hook' name should refer to a builtin sort; if it is unset or the name is
  not recognized, verification succeeds.

 -}
sortDeclVerifier :: Verifiers -> Hook -> SortDeclVerifier
sortDeclVerifier Verifiers { sortDeclVerifiers } hook =
    let
        hookedSortVerifier :: Maybe SortDeclVerifier
        hookedSortVerifier = do
            -- Get the builtin sort name.
            sortName <- getHook hook
            HashMap.lookup sortName sortDeclVerifiers
    in
        case hookedSortVerifier of
            Nothing ->
                -- There is nothing to verify because either
                -- 1. the sort is not hooked, or
                -- 2. there is no SortVerifier registered to the hooked name.
                -- In either case, there is nothing more to do.
                \_ -> pure ()
            Just verifier ->
                -- Invoke the verifier that is registered to this builtin sort.
                verifier

{- | Look up and apply a builtin symbol verifier.

  The 'Hook' name should refer to a builtin symbol; if it is unset or the name is
  not recognized, verification succeeds.

 -}
symbolVerifier :: Verifiers -> Hook -> SymbolVerifier
symbolVerifier Verifiers { symbolVerifiers } hook =
    let
        hookedSymbolVerifier :: Maybe SymbolVerifier
        hookedSymbolVerifier = do
            -- Get the builtin sort name.
            symbolName <- getHook hook
            HashMap.lookup symbolName symbolVerifiers
    in
        case hookedSymbolVerifier of
            Nothing ->
                -- There is nothing to verify because either
                -- 1. the symbol is not hooked, or
                -- 2. there is no SymbolVerifier registered to the hooked name.
                -- In either case, there is nothing more to do.
                \_ _ -> pure ()
            Just verifier ->
                -- Invoke the verifier that is registered to this builtin symbol.
                verifier

notImplemented :: Function
notImplemented =
    ApplicationFunctionEvaluator notImplemented0
  where
    notImplemented0 _ _ _ = pure (NotApplicable, SimplificationProof)

{- | Verify a builtin sort declaration.

  Check that the hooked sort does not take any sort parameters.

 -}
verifySortDecl :: SortDeclVerifier
verifySortDecl
    SentenceSort
    { sentenceSortName = sortId@Id { getId = sortName }
    , sentenceSortParameters
    }
  =
    withLocationAndContext
    sortId
    ("In sort '" ++ sortName ++ "' declaration")
    (case sentenceSortParameters of
        [] -> pure ()
        _ ->
            Kore.Error.koreFail
                ("Expected 0 sort parameters, found "
                    ++ show (length sentenceSortParameters))
    )

{- | Verify the occurrence of a builtin sort.

  Check that the sort is hooked to the named builtin. The sort parameters are
  already checked by the verifier.

 -}
verifySort
    :: (Id Object -> Either (Error VerifyError) (SortDescription Object))
    -> String
    -> Sort Object
    -> Either (Error VerifyError) ()
verifySort findSort builtinName (SortActualSort SortActual { sortActualName }) =
    do
        SentenceSort { sentenceSortAttributes } <- findSort sortActualName
        let expectHook = Hook (Just builtinName)
        declHook <- Kore.Error.castError (parseAttributes sentenceSortAttributes)
        Kore.Error.koreFailWhen (expectHook /= declHook)
            ("Sort '" ++ getId sortActualName
                ++ "' is not hooked to builtin sort '"
                ++ builtinName ++ "'")
verifySort _ _ (SortVariableSort SortVariable { getSortVariable }) =
    Kore.Error.koreFail
        ("unexpected sort variable '" ++ getId getSortVariable ++ "'")

{- | Verify a builtin symbol declaration.

  The declared sorts must match the builtin sorts.

  See also: 'verifySymbolArguments'

 -}
verifySymbol
    :: SortVerifier  -- ^ Builtin result sort
    -> [SortVerifier]  -- ^ Builtin argument sorts
    -> SymbolVerifier
verifySymbol
    verifyResult
    verifyArguments
    findSort
    decl@SentenceSymbol
        { sentenceSymbolSymbol =
            Symbol { symbolConstructor = symbolId@Id { getId = symbolName } }
        , sentenceSymbolResultSort = result
        }
  =
    withLocationAndContext
        symbolId
        ("In symbol '" ++ symbolName ++ "' declaration")
        (do
            Kore.Error.withContext "In result sort"
                (verifyResult findSort result)
            verifySymbolArguments verifyArguments findSort decl
        )

{- | Verify the arguments of a builtin sort declaration.

  The declared argument sorts must match the builtin argument
  sorts. @verifySymbolArguments@ only checks the symbol's argument sorts; use
  'verifySymbol' if it is also necessary to check the symbol's result sort.

  See also: 'verifySymbol'

 -}
verifySymbolArguments
    :: [SortVerifier]  -- ^ Builtin argument sorts
    -> SymbolVerifier
verifySymbolArguments
    verifyArguments
    findSort
    SentenceSymbol { sentenceSymbolSorts = sorts }
  =
    Kore.Error.withContext "In argument sorts"
    (do
        Kore.Error.koreFailWhen (arity /= builtinArity)
            ("Expected " ++ show builtinArity
             ++ " arguments, found " ++ show arity)
        zipWithM_ (\verify sort -> verify findSort sort) verifyArguments sorts
    )
  where
    builtinArity = length verifyArguments
    arity = length sorts

{- | Verify a domain value pattern.

  If the given pattern is not a domain value, it is skipped.

  See also: 'verifyStringLiteral'

 -}
verifyDomainValue
    :: String  -- ^ Builtin sort name
    -> DomainValueVerifier
    -- ^ validation function
    -> PatternVerifier
verifyDomainValue builtinSort validate =
    PatternVerifier { runPatternVerifier }
  where
    runPatternVerifier
        :: (Id Object -> Either (Error VerifyError) (SortDescription Object))
        -- ^ Function to lookup sorts by identifier
        -> Pattern Object Variable CommonKorePattern
        -- ^ Pattern to verify
        -> Either (Error VerifyError) ()
    runPatternVerifier findSort =
        \case
            DomainValuePattern dv@DomainValue { domainValueSort } ->
                Kore.Error.withContext
                    ("Verifying builtin sort '" ++ builtinSort ++ "'")
                    (skipOtherSorts domainValueSort (validate dv))
            _ -> return ()  -- no domain value to verify
      where
        -- | Run @next@ if @sort@ is hooked to @builtinSort@; do nothing
        -- otherwise.
        skipOtherSorts
            :: Sort Object
            -- ^ Sort of pattern under verification
            -> Either (Error VerifyError) ()
            -- ^ Verifier run iff pattern sort is hooked to designated builtin
            -> Either (Error VerifyError) ()
        skipOtherSorts sort next = do
            decl <-
                Except.catchError
                    (Just <$> verifySort findSort builtinSort sort)
                    (\_ -> return Nothing)
            case decl of
              Nothing -> return ()
              Just () -> next

{- | Verify a literal string domain value.

  If the given domain value is not a literal string, it is skipped.

  See also: 'verifyDomainValue'

 -}
verifyStringLiteral
    :: (StringLiteral -> Either (Error VerifyError) ())
    -- ^ validation function
    -> DomainValueVerifier
verifyStringLiteral validate DomainValue { domainValueChild } =
    case Functor.Foldable.project domainValueChild of
        StringLiteralPattern lit@StringLiteral {} -> validate lit
        _ -> return ()

{- | Run a parser in a domain value pattern.

  An error is thrown if the domain value does not contain a literal string.
  The parsed value is returned.

 -}
parseDomainValue
    :: Parser a
    -> DomainValue Object (CommonPurePattern Meta)
    -> Either (Error VerifyError) a
parseDomainValue
    parser
    DomainValue { domainValueChild }
  =
    Kore.Error.withContext "While parsing domain value"
        (case domainValueChild of
            StringLiteral_ StringLiteral { getStringLiteral = lit } ->
                let parsed =
                        Parsec.parse
                            (parser <* Parsec.eof)
                            "<string literal>"
                            lit
                in castParseError parsed
            _ -> Kore.Error.koreFail "Expected literal string"
        )
  where
    castParseError =
        either (Kore.Error.koreFail . Parsec.parseErrorPretty) pure

{- | Return the supplied pattern as an 'AttemptedFunction'.

  No substitution or predicate is applied.

  See also: 'ExpandedPattern'
 -}
appliedFunction
    :: Monad m
    => CommonPurePattern Object
    -> m (AttemptedFunction Object Variable)
appliedFunction term =
    (return . Applied . OrOfExpandedPattern.make)
        [ExpandedPattern
            { term
            , predicate = Predicate.makeTruePredicate
            , substitution = []
            }
        ]

{- | Look up the result sort of a symbol or alias
 -}
getResultSort :: MetadataTools level attrs -> SymbolOrAlias level -> Sort level
getResultSort MetadataTools { sortTools } symbol =
    case sortTools symbol of
        ApplicationSorts { applicationSortsResult } -> applicationSortsResult

{- | Construct a builtin binary operator.

  Both operands have the same builtin type, which may be different from the
  result type.

  The function is skipped if its arguments are not domain values.
  It is an error if the wrong number of arguments is given; this must be checked
  during verification.

 -}
binaryOperator
    :: Parser a
    -- ^ Parse operand
    -> (Sort Object -> b -> CommonPurePattern Object)
    -- ^ Render result as pattern with given sort
    -> String
    -- ^ Builtin function name (for error messages)
    -> (a -> a -> b)
    -- ^ Operation on builtin types
    -> Function
binaryOperator
    parser
    asPattern
    ctx
    op
  =
    ApplicationFunctionEvaluator binaryOperator0
  where
    get = Except.liftEither . Kore.Error.castError . parseDomainValue parser
    binaryOperator0
        tools
        _
        Application
            { applicationSymbolOrAlias =
                (getResultSort tools -> resultSort)
            , applicationChildren
            }
      =
        Kore.Error.withContext ctx
            (case Functor.Foldable.project <$> applicationChildren of
                [DomainValuePattern a, DomainValuePattern b] -> do
                    -- Apply the operator to two domain values
                    r <- op <$> get a <*> get b
                    (,)
                        <$> (appliedFunction . asPattern resultSort) r
                        <*> pure SimplificationProof
                [_, DomainValuePattern _] ->
                    Kore.Error.withContext
                        "In first argument"
                        expectedDomainValue
                [DomainValuePattern _, _] ->
                    Kore.Error.withContext
                        "In second argument"
                        expectedDomainValue
                _ -> wrongArity
            )

{- | Construct a builtin unary operator.

  The operand type may differ from the result type.

  The function is skipped if its arguments are not domain values.
  It is an error if the wrong number of arguments is given; this must be checked
  during verification.

 -}
unaryOperator
    :: Parser a
    -- ^ Parse operand
    -> (Sort Object -> b -> CommonPurePattern Object)
    -- ^ Render result as pattern with given sort
    -> String
    -- ^ Builtin function name (for error messages)
    -> (a -> b)
    -- ^ Operation on builtin types
    -> Function
unaryOperator
    parser
    asPattern
    ctx
    op
  =
    ApplicationFunctionEvaluator unaryOperator0
  where
    get = Except.liftEither . Kore.Error.castError . parseDomainValue parser
    unaryOperator0
        tools
        _
        Application
            { applicationSymbolOrAlias =
                (getResultSort tools -> resultSort)
            , applicationChildren
            }
      =
        Kore.Error.withContext ctx
            (case Functor.Foldable.project <$> applicationChildren of
                [DomainValuePattern a] -> do
                    -- Apply the operator to a domain value
                    r <- op <$> get a
                    (,)
                        <$> (appliedFunction . asPattern resultSort) r
                        <*> pure SimplificationProof
                [_] ->
                    Kore.Error.withContext
                        "In first argument"
                        expectedDomainValue
                _ -> wrongArity
            )

expectedDomainValue :: MonadError (Error w) m => m a
expectedDomainValue = Kore.Error.koreFail "Expected domain value"

wrongArity :: MonadError (Error w) m => m a
wrongArity = Kore.Error.koreFail "Wrong number of arguments"
