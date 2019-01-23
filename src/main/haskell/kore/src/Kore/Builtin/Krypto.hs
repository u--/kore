{- |
Module      : Kore.Builtin.Krypto
Description : Built-in cryptographic functions.
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : vladimir.ciobanu@runtimeverification.com
Stability   : experimental
Portability : portable

This module is intended to be imported qualified, to avoid collision with other
builtin modules.

@
    import qualified Kore.Builtin.Krypto as Krypto
@
 -}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Kore.Builtin.Krypto
    ( symbolVerifiers
    , builtinFunctions
    , keccakKey
<<<<<<< HEAD
=======
    , keccakKeyT
    , signatureToKey
>>>>>>> Implemented KRYPTO.ecdsaRecover hook
    ) where

import           Crypto.Hash
                 ( Digest, Keccak_256, hash )
<<<<<<< HEAD
=======
import           Crypto.PubKey.ECC.Prim
import           Crypto.PubKey.ECC.Types
import           Data.Bits
import           Data.ByteString
                 ( ByteString )
import qualified Data.ByteString as ByteString
import           Data.ByteString.Char8
                 ( pack )
import           Data.Char
>>>>>>> Implemented KRYPTO.ecdsaRecover hook
import qualified Data.HashMap.Strict as HashMap
import           Data.Map
                 ( Map )
import qualified Data.Map as Map
import           Data.String
                 ( IsString, fromString )
import           Data.Text
                 ( Text )
<<<<<<< HEAD
import qualified Data.Text.Encoding as Text
=======
import           Data.Word
                 ( Word8 )
>>>>>>> Implemented KRYPTO.ecdsaRecover hook

import qualified Kore.Builtin.Builtin as Builtin
import qualified Kore.Builtin.Int as Int
import qualified Kore.Builtin.String as String

import Control.Exception.Base
       ( assert )

keccakKey :: IsString s => s
keccakKey = "KRYPTO.keccak256"

ecsdaRecover :: String
ecsdaRecover = "KRYPTO.ecdsaRecover"

ecsdaRecoverT :: Text
ecsdaRecoverT = "KRYPTO.ecdsaRecover"

{- | Verify that hooked symbol declarations are well-formed.

  See also: 'Builtin.verifySymbol'

-}
symbolVerifiers :: Builtin.SymbolVerifiers
symbolVerifiers =
    HashMap.fromList
    [ ( keccakKey
      , Builtin.verifySymbol String.assertSort [String.assertSort]
      )
    , (ecsdaRecoverT
      , Builtin.verifySymbol
            String.assertSort
            [ String.assertSort
            , Int.assertSort
            , String.assertSort
            , String.assertSort
            ]
      )
    ]

{- | Implement builtin function evaluation.
 -}
builtinFunctions :: Map Text Builtin.Function
builtinFunctions =
    Map.fromList
        [ (keccakKey, evalKeccak)
        , (ecsdaRecoverT, evalECDSARecover)
        ]

evalKeccak :: Builtin.Function
evalKeccak =
    Builtin.functionEvaluator evalKeccak0
  where
    evalKeccak0 :: Builtin.FunctionImplementation
    evalKeccak0 _ _ resultSort arguments =
        Builtin.getAttemptedFunction $ do
            let
                arg =
                    case arguments of
                      [input] -> input
                      _ -> Builtin.wrongArity keccakKey
            str <- String.expectBuiltinString keccakKey arg
            let
                digest = hash . Text.encodeUtf8 $ str :: Digest Keccak_256
                result = fromString (show digest)
            Builtin.appliedFunction
                $ String.asExpandedPattern resultSort result

evalECDSARecover :: Builtin.Function
evalECDSARecover =
    Builtin.functionEvaluator eval0

eval0 :: Builtin.FunctionImplementation
eval0 _ _ resultSort [messageHash0, v0, r0, s0] =
    Builtin.getAttemptedFunction $ do
        messageHash <- s2i <$> String.expectBuiltinString "" messageHash0
        v <- Int.expectBuiltinInt "" v0
        r <- s2i <$> String.expectBuiltinString "" r0
        s <- s2i <$> String.expectBuiltinString "" s0
        Builtin.appliedFunction
            $ String.asExpandedPattern resultSort
            $ bs2s
            $ pad 64 0
            $ signatureToKey messageHash r s v
eval0 _ _ _ _ = Builtin.wrongArity ecsdaRecover

pad :: Int -> Word8 -> ByteString -> ByteString
pad n w s = ByteString.append s padding
  where
    padding =
        ByteString.take (n - ByteString.length s)
      $ ByteString.pack (repeat w)

signatureToKey
    :: Integer
    -> Integer
    -> Integer
    -> Integer
    -> ByteString
signatureToKey messageHash r s v =
    assert (28 <= v && v <= 34)
  $ ByteString.drop 1
  $ encodePoint compressed
  $ recoverPublicKey recId (r, s) messageHash
  where
    recId = v - 27
    compressed = v >= 31

recoverPublicKey
    :: Integer
    -> (Integer, Integer)
    -> Integer
    -> Point
recoverPublicKey recId (r, s) e =
    assert (recId > 0)
  $ assert (r > 0)
  $ assert (s > 0)
  $ assert (pt_x < p)
  $ assert (pointMul p256k1 n pt == PointO)
  $ pointAddTwoMuls
            p256k1
            (mulMod n (invMod n r) s)
            pt
            (mulMod n (invMod n r) (n - e `mod` n))
            (ecc_g curveParams)
  where
    p256k1@(CurveFP (CurvePrime p curveParams)) = getCurveByName SEC_p256k1

    n = ecc_n curveParams

    i = recId `div` 2

    pt_x = r + i*n

    pt = decompressPt pt_x (recId .&. 1 == 1)

    decompressPt x signBit = Point x (if signBit then y else p - y)
      where
        y = sqrtMod p $
              (powMod p x 3)
            + (mulMod p (ecc_a curveParams) x)
            + (ecc_b curveParams)

invMod
    :: Integer
    -> Integer
    -> Integer
invMod p x = powMod p x (p - 2)

sqrtMod
    :: Integer
    -> Integer
    -> Integer
sqrtMod p x = powMod p x ((p + 1) `div` 4)

mulMod
    :: Integer
    -> Integer
    -> Integer
    -> Integer
mulMod p x y = (x * y) `mod` p

powMod
    :: Integer
    -> Integer
    -> Integer
    -> Integer
powMod p _ 0 = 1
powMod p x a =
    mulMod p
    (if even a then 1 else x)
    (powMod p (mulMod p x x) (a `div` 2))

-- Leading bit signals whether the point is compressed.
-- Superfluous because we drop it later on.
-- Kept here for completeness sake, to match
-- the code in the java backend.
encodePoint
    :: Bool
    -> Point
    -> ByteString
encodePoint compressed (Point x y)
    | compressed =
        ByteString.cons
        (if even y then 0x02 else 0x03)
        (i2bs x)
    | otherwise = ByteString.concat
        [ ByteString.pack [0x04]
        , i2bs x
        , i2bs y
        ]
encodePoint _ _ = error "Should never obtain point-at-infinity here!"

bs2s :: ByteString -> String
bs2s = map (chr . fromIntegral) . ByteString.unpack

s2i :: String -> Integer
s2i = bs2i . ByteString.pack . map (fromIntegral . ord)

bs2i :: ByteString -> Integer
bs2i =
    ByteString.foldr (\word i -> fromIntegral word + (i `shiftL` 8)) 0
  . ByteString.reverse

i2bs :: Integer -> ByteString
i2bs = ByteString.reverse .
    ByteString.unfoldr
    (\i -> if i == 0
        then Nothing
        else Just (fromIntegral (i `mod` 256), i `div` 256)
    )
