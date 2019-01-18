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

module Kore.Builtin.Krypto
    ( symbolVerifiers
    , builtinFunctions
    , keccakKey
    , keccakKeyT
    ) where

import           Crypto.Hash
                 ( Digest, Keccak_256, hash )
import           Crypto.PubKey.ECC.Prim
import           Crypto.PubKey.ECC.Types
import           Data.Char
import           Data.Bits
import qualified Data.ByteString as ByteString
import           Data.ByteString
                 ( ByteString )
import           Data.ByteString.Char8
                 ( pack )
import           Data.Word
                 ( Word8 )
import qualified Data.HashMap.Strict as HashMap
import           Data.Map
                 ( Map )
import qualified Data.Map as Map
import           Data.Text
                 ( Text )

import qualified Kore.Builtin.Builtin as Builtin
import qualified Kore.Builtin.String as String
import qualified Kore.Builtin.Int as Int


keccakKey :: String
keccakKey = "KRYPTO.keccak256"

keccakKeyT :: Text
keccakKeyT = "KRYPTO.keccak256"

ecsdaRecover :: String
ecsdaRecover = "KRYPTO.ecsdaRecover"

ecsdaRecoverT :: Text
ecsdaRecoverT = "KRYPTO.ecsdaRecover"

{- | Verify that hooked symbol declarations are well-formed.

  See also: 'Builtin.verifySymbol'

-}
symbolVerifiers :: Builtin.SymbolVerifiers
symbolVerifiers =
    HashMap.fromList
    [ ( keccakKeyT
      , Builtin.verifySymbol String.assertSort [String.assertSort]
      )
    ]

{- | Implement builtin function evaluation.
 -}
builtinFunctions :: Map Text Builtin.Function
builtinFunctions =
    Map.fromList
        [ (keccakKeyT, evalKeccak)
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
                digest = hash . pack $ str :: Digest Keccak_256
                result = show digest
            Builtin.appliedFunction
                $ String.asExpandedPattern resultSort result

evalECDSARecover :: Builtin.Function
evalECDSARecover = 
    Builtin.functionEvaluator eval0
  where
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
                $ ByteString.drop 1 
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
signatureToKey messageHash r s v | good = encodePoint compressed $
    recoverPublicKey recId (r, s) messageHash
  where
    recId = v - 27
    compressed = v >= 31
    good = 28 <= v && v <= 34
signatureToKey _ _ _ _ = error "v out of bounds"

recoverPublicKey 
    :: Integer 
    -> (Integer, Integer) 
    -> Integer 
    -> Point
recoverPublicKey recId (r, s) e | good = q
  where
    p256k1@(CurveFP (CurvePrime p curveParams)) = getCurveByName SEC_p256k1

    invMod x = powMod x (p - 2)

    sqrtMod x = powMod x ((p + 1) `div` 4)

    mulMod x y = (x * y) `mod` p

    powMod _ 0 = 1
    powMod x a = 
        mulMod
        (if even a then 1 else x) 
        (powMod (x * x `mod` p) (a `div` 2))

    decompressPt x signBit = Point x (if signBit then y else p - y)
      where 
        y = sqrtMod $ 
              (x `mulMod` x `mulMod` x) 
            + (ecc_a curveParams `mulMod` x) 
            + (ecc_b curveParams)

    n = ecc_n curveParams
    i = recId `div` 2
    pt_x = r + i*n
    pt = decompressPt pt_x (recId .&. 1 == 1)
    q = pointMul p256k1 (invMod r) $
        pointAddTwoMuls 
            p256k1 
            s
            (ecc_g curveParams)
            (p - e `mod` p)
            pt
    good = 
        recId > 0 
     && r > 0 
     && s > 0 
     && pt_x < p 
     && pointMul p256k1 n pt == PointO
recoverPublicKey _ _ _ = error "Bad input"

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
bs2s = map (intToDigit . fromIntegral) . ByteString.unpack

s2i :: String -> Integer
s2i = bs2i . ByteString.pack . map (fromIntegral . digitToInt)

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
