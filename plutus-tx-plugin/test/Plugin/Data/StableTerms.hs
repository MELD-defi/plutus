{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-context #-}
-- We don't truncate types for terms that are compiled separately, to be applied afterward in other test cases.
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-truncate-types #-}

module Plugin.Data.StableTerms
    ( stableListConstruct
    , stableTrue
    , stableFalse
    , stableAnd
    , stableTupleMatch
    , stableMonoCase
    , stableListMatch
    , stablePtreeMatch
    , stablePolyRec
    , stableDefaultCasePoly
    , stableErrorPlc
    , stablePtreeFirst
    , stableJoinError
    , stableSumDirect
    ) where

import           Data.Proxy        (Proxy (..))
import           Plugin.Lib        (joinError)
import qualified PlutusTx.Builtins as Builtins
import           PlutusTx.Code     (CompiledCode)
import           PlutusTx.Plugin   (plc)

stableListConstruct :: CompiledCode [Integer]
stableListConstruct = plc (Proxy @"stableListConstruct") ([]::[Integer])

stableTrue :: CompiledCode Bool
stableTrue = plc (Proxy @"stableTrue") True

stableFalse :: CompiledCode Bool
stableFalse = plc (Proxy @"stableFalse") False

stableAnd :: CompiledCode (Bool -> Bool -> Bool)
stableAnd = plc (Proxy @"stableAnd") (\(x::Bool) (y::Bool) -> if x then (if y then True else False) else False)

stableTupleMatch :: CompiledCode ((Integer, Integer) -> Integer)
stableTupleMatch = plc (Proxy @"stableTupleMatch") (\(x:: (Integer, Integer)) -> let (a, _) = x in a)

data MyMonoData = Mono1 Integer Integer | Mono2 Integer | Mono3 Integer
    deriving (Show, Eq)

stableMonoCase :: CompiledCode (MyMonoData -> Integer)
stableMonoCase = plc (Proxy @"stableMonoCase") (\(x :: MyMonoData) -> case x of { Mono1 _ b -> b;  Mono2 a -> a; Mono3 a -> a })

stableListMatch :: CompiledCode ([Integer] -> Integer)
stableListMatch = plc (Proxy @"stableListMatch") (\(l::[Integer]) -> case l of { (x:_) -> x ; [] -> 0; })

data B a = One a | Two (B (a, a))

-- TODO: replace this with 'first' when we have working recursive functions
stablePtreeMatch :: CompiledCode (B Integer -> Integer)
stablePtreeMatch = plc (Proxy @"stablePtreeMatch") (\(t::B Integer) -> case t of { One a -> a; Two _ -> 2; })

stablePtreeFirst :: CompiledCode (B Integer -> Integer)
stablePtreeFirst = plc (Proxy @"stablePtreeFirst") (
    let go :: (a -> Integer) -> B a -> Integer
        go k (One x) = k x
        go k (Two b) = go (\(x, _) -> k x) b
    in go (\x -> x))

stablePolyRec :: CompiledCode (B Integer -> Integer)
stablePolyRec = plc (Proxy @"stablePolyRec") (
    let
        depth :: B a -> Integer
        depth tree = case tree of
            One _     -> 1
            Two inner -> Builtins.addInteger 1 (depth inner)
    in \(t::B Integer) -> depth t)

data MyPolyData a b = Poly1 a b | Poly2 a

stableDefaultCasePoly :: CompiledCode (MyPolyData Integer Integer -> Integer)
stableDefaultCasePoly = plc (Proxy @"stableDefaultCasePoly") (\(x :: MyPolyData Integer Integer) -> case x of { Poly1 a _ -> a ; _ -> 2; })

stableErrorPlc :: CompiledCode (() -> Integer)
stableErrorPlc = plc (Proxy @"stableErrorPlc") (Builtins.error @Integer)

stableJoinError :: CompiledCode (Bool -> Bool -> ())
stableJoinError = plc (Proxy @"stableJoinError") joinError

stableSumDirect :: CompiledCode ([Integer] -> Integer)
stableSumDirect = plc (Proxy @"stableSumDirect") (
    let sum :: [Integer] -> Integer
        sum []     = 0
        sum (x:xs) = Builtins.addInteger x (sum xs)
    in sum)
