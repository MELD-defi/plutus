{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeOperators    #-}
-- | Optimization passes for removing dead code, mainly dead let bindings.
module PlutusIR.Transform.DeadCode (removeDeadBindings) where

import           PlutusIR
import qualified PlutusIR.Analysis.Dependencies as Deps
import           PlutusIR.MkPir
import           PlutusIR.Transform.Rename      ()

import qualified PlutusCore                     as PLC
import qualified PlutusCore.Constant            as PLC
import qualified PlutusCore.StdLib.Data.Unit    as Unit

import           Control.Lens
import           Control.Monad.Extra            (mapMaybeM)
import           Control.Monad.State

import           Data.Coerce
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set

import qualified Algebra.Graph                  as G
import qualified Algebra.Graph.ToGraph          as T
import qualified Data.List.NonEmpty             as NE

import           Debug.Trace
import qualified PlutusCore.Name                as PLC

type TruncateTypes = Bool

-- | Remove all the dead let bindings in a term.
removeDeadBindings ::
    ( PLC.HasUnique name PLC.TermUnique
    , PLC.HasUnique tyname PLC.TypeUnique
    , PLC.ToBuiltinMeaning uni fun
    , PLC.MonadQuote m
    , PLC.GShow uni
    , PLC.Closed uni
    , Show a
    , Show name
    , Show tyname
    , Show fun
    , PLC.Everywhere uni Show
    , PLC.Contains uni ()
    , (TermLike (Term tyname name uni fun) TyName Name uni fun)
    )
    => TruncateTypes
    -> Term tyname name uni fun a
    -> m (Term tyname name uni fun a)
removeDeadBindings tt t = do
    tRen <- PLC.rename t
    fst <$> runStateT (transformMOf termSubterms processTerm tRen)
                      (Ctx (calculateLiveness tRen) [] tt)

type Liveness = (Set.Set Deps.Node, Destructors, DeadUniques)
type Destructors = Map.Map PLC.Unique [Bool]
type DeadUniques = [PLC.Unique]
type CurrentConstructors = [Bool]
data Ctx = Ctx Liveness CurrentConstructors TruncateTypes

calculateLiveness ::
    ( PLC.HasUnique name PLC.TermUnique
    , PLC.HasUnique tyname PLC.TypeUnique
    , PLC.ToBuiltinMeaning uni fun
    )
    => Term tyname name uni fun a
    -> Liveness
calculateLiveness t =
    let depGraph :: G.Graph Deps.Node
        (depGraph, (_, destructors, dataTypes, vars)) = Deps.runTermDeps t
        liveness = Set.fromList $ T.reachable Deps.Root depGraph
        l u = Set.member (Deps.Variable u) liveness
        ds = fmap (fmap l) destructors
        dt = fst <$> filter (\(_, d) -> not (or (ds Map.! d))) dataTypes
        dv = fst <$> filter (\(_, d) -> d `elem` dt) vars
    in (liveness, ds, dt ++ dv)

live :: (MonadState Ctx m, PLC.HasUnique n unique) => n -> m Bool
live n = do
    Ctx (liveness, _, du) _ tt <- get
    let u = coerce $ n ^. PLC.unique
    pure $ Set.member (Deps.Variable u) liveness && (not tt || u `notElem` du)

-- FIXME: Don't even consider this trash
liveBinding ::
    ( MonadState Ctx m
    , PLC.HasUnique name PLC.TermUnique
    , PLC.HasUnique tyname PLC.TypeUnique
    , Show name
    )
    => Binding tyname name uni fun a
    -> m (Maybe (Binding tyname name uni fun a))
liveBinding b = do
    Ctx _ _ truncateTypes <- get
    let -- TODO: HasUnique instances for VarDecl and TyVarDecl?
        liveVarDecl (VarDecl _ n _) = live n
        liveTyVarDecl (TyVarDecl _ n _) = live n
        f True = Just b
        f _    = Nothing
    case b of
        TermBind _ _ d _ -> f <$> liveVarDecl d
        TypeBind _ d _ -> f <$> liveTyVarDecl d
        DatatypeBind a (Datatype a' d t destr constrs) ->
            if truncateTypes -- Allow the removal of unused constructors
            then liveTyVarDecl d >>= \case
                True -> filterM liveVarDecl constrs >>= \constrs' ->
                    pure $ if null constrs'
                        then Nothing
                        else Just (DatatypeBind a (Datatype a' d t destr constrs'))
                _ -> pure Nothing
            else pure (Just b)

processTerm ::
    ( MonadState Ctx m
    , PLC.HasUnique name PLC.TermUnique
    , PLC.HasUnique tyname PLC.TypeUnique
    , PLC.GShow uni
    , PLC.Closed uni
    , Show a
    , Show name
    , Show tyname
    , Show fun
    , PLC.Everywhere uni Show
    , PLC.Contains uni ()
    , (TermLike (Term tyname name uni fun) TyName Name uni fun)
    )
    => Term tyname name uni fun a
    -> m (Term tyname name uni fun a)
processTerm = \case
    -- throw away dead bindings
    Let x r bs t -> mkLet x r <$> mapMaybeM liveBinding (NE.toList bs) <*> pure t
    -- throw away dead vars
    ot@(Var x n) -> do
        Ctx (_, _, dts) _ tt <- get
        l <- live n
        pure $ if tt && n ^. PLC.theUnique `elem` dts
            then x <$ Unit.unitval
            else ot
    -- throw away dead types
    ot@(LamAbs x _ (TyVar _ n) _) -> do
        Ctx (_, _, dts) _ tt <- get
        pure $ if tt && n ^. PLC.theUnique `elem` dts
            then x <$ Unit.unitval
            else ot
    ot@(LamAbs x _ (TyApp _ (TyVar _ n) _) _) -> do
        Ctx (_, _, dts) _ tt <- get
        pure $ if tt && n ^. PLC.theUnique `elem` dts
            then x <$ Unit.unitval
            else ot
    ot@(LamAbs x _ (TyApp _ (TyApp _ (TyVar _ n) _) _) _) -> do
        Ctx (_, _, dts) _ tt <- get
        pure $ if tt && n ^. PLC.theUnique `elem` dts
            then x <$ Unit.unitval
            else ot
    ot@(LamAbs x _ (TyApp _ (TyLam _ _ _ (TyApp _ (TyVar _ n) _)) _) _) -> do
        Ctx (_, _, dts) _ tt <- get
        pure $ if tt && n ^. PLC.theUnique `elem` dts
            then x <$ Unit.unitval
            else ot
    -- throw away destructor branches of unused constructors
    ot@(Apply _ (Var _ n) _) -> do
        Ctx (l, ds, dt) _ tt <- get
        when (tt && (n ^. PLC.theUnique) `Map.member` ds) $ do
            -- traceShow (n, Map.lookup (n ^. PLC.theUnique) ds) (pure ())
            put (Ctx (l, ds, dt) (ds Map.! (n ^. PLC.theUnique)) tt)
        pure ot
    ot@(Apply _ f _) -> do
        Ctx (l, ds, dt) cs tt <- get
        if null cs
            then pure ot
            else do
                let c : cs' = cs
                put (Ctx (l, ds, dt) cs' tt)
                if c
                    then pure ot
                    else pure f
    x -> pure x
