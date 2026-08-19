{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications, ScopedTypeVariables, BangPatterns, MultiWayIf, OverloadedRecordDot #-}
module GHC.Debugger.Stopped.Frames
 ( getStackFrameBindings
 , addIdsToInteractiveContext
 )
 where

import Control.Monad
import Control.Monad.Reader
import qualified Data.List as L
import qualified Data.Map.Strict as Map

import GHC
import GHC.ByteCode.Breakpoints
import GHC.Data.Maybe
import GHC.Driver.Env as GHC
import GHC.Runtime.Eval
import GHC.Utils.Outputable as Ppr

import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Utils
import qualified Colog.Core as Logger
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Utils.Monad as GHC
import qualified GHC.IfaceToCore as GHC
import qualified GHC.Linker.Loader as Loader
import qualified GHC.Debugger.Runtime.Eval.RemoteExpr as Remote
import qualified GHC.Exts.Heap.Closures as GHC
import qualified GHC.Types.Id as Id
import GHC.Iface.Env (newInteractiveBinder)
import qualified GHC.Runtime.Context as GHC
import qualified GHC.Core.Predicate as GHC
import qualified GHC.Core.TyCo.Tidy as GHC
import qualified GHC.Types.RepType as GHC
import qualified GHC.Tc.Utils.TcType as GHC
import qualified GHC.Utils.Logger as GHC
import qualified GHC.Core.TyCo.Ppr as GHC
import qualified GHC.Runtime.Heap.Inspect as GHC
import GHCi.RemoteTypes (ForeignRef)
#if MIN_VERSION_ghc(9,14,2)
import GHC.Linker.Types
#endif

-- We need a fresh Unique for each Id we bind, because the linker
-- state is single-threaded and otherwise we'd spam old bindings
-- whenever we stop at a breakpoint.  The InteractveContext is properly
-- saved/restored, but not the linker state.  See #1743, test break026.
mkNewId :: HscEnv -> GHC.FastString -> GHC.Type -> Maybe Id -> IO Id
mkNewId hsc_env occ ty old_id
  = do { name <- newInteractiveBinder hsc_env (GHC.mkVarOccFS occ) (fromMaybe GHC.interactiveSrcSpan $ GHC.getSrcSpan <$> old_id)
          -- NB: use variable namespace.
          -- Don't use record field namespaces, lest we cause #25109.
      ; return $ Id.mkVanillaGlobalWithInfo name ty (fromMaybe GHC.vanillaIdInfo $ GHC.idInfo <$> old_id) }

getStackFrameBindings :: DbgStackFrame -> Debugger [Id]
getStackFrameBindings frame@DbgStackFrame{breakId = ibi,args = Nothing} = do
  logSDoc Logger.Warning $ text "getStackFrameBindings: no args. ibi,frame =" <+> ppr ibi <+> text "," <+> text (show frame)
  return []
getStackFrameBindings DbgStackFrame{breakId = ibi0, args = Just (DbgStackFrameBCOArgs (NoShow bcoArgsRef) offset0)}  = do
  case (ibi0, offset0) of
    (Just ibi, Just offset)
      -> do
#ifdef TESTING
      -- Making sure the fallback path doesn't crash.
      -- It was hard to directly trigger.
      fids <- bindFrameVarsWithNoInfo bcoArgsRef
      logSDoc Logger.Debug $ text "fallback ids" <+> ppr fids
#endif
      bindFrameVarsWithBreakpointInfo ibi bcoArgsRef offset
    _ -> bindFrameVarsWithNoInfo bcoArgsRef

bindFrameVarsWithNoInfo :: ForeignRef a -> Debugger [Id]
bindFrameVarsWithNoInfo bcoArgsRef = do
    bcoArgs <- (expectRight =<<) $ Remote.evalIOList $
      Remote.raw "\\ f xs -> Prelude.mapM f xs :: IO [GHCi.RemoteTypes.HValue]"
      `Remote.app` unpackStackField
      `Remote.appRef` bcoArgsRef
    hsc_env <- getSession
    let artificial = zipWith fa bcoArgs [0 :: Int ..]
          where
            fa fv i = do
              id' <- mkNewId hsc_env (GHC.mkFastString $ "_a" ++ show i) (GHC.anyTypeOfKind GHC.liftedTypeKind) Nothing
              pure $ (id', fv, getOccName id')
    arts <- liftIO $ sequence artificial
    liftIO $ bindForeignHValues hsc_env arts

bindFrameVarsWithBreakpointInfo :: InternalBreakpointId -> ForeignRef [GHC.StackField] -> Word -> Debugger [Id]
bindFrameVarsWithBreakpointInfo ibi bcoArgs delta0 = do
  hsc_env <- getSession
  let hug = hsc_HUG hsc_env

  info_brks <- liftIO $ readIModBreaks hug ibi
  occs <- liftIO $ getBreakVars (readIModModBreaks hug) ibi info_brks
  let info  = getInternalBreak ibi info_brks
  let delta = fromIntegral delta0
  (mbVars, _result_ty) <- liftIO $ GHC.initIfaceLoad hsc_env
                    $ GHC.initIfaceLcl (ibi_info_mod ibi) (text "debugger") NotBoot
                    $ GHC.hydrateCgBreakInfo info
  unless (length mbVars == length occs) $ do
    logSDoc Logger.Warning $ text "different length of cgb_vars and getBreakVars for ibi" <+> ppr mbVars <+> ppr occs <+> ppr ibi
  let mbVarsIx = flip map mbVars $ \x -> x >>= \(var,offset) -> (var,) <$> do
        guard (offset >= delta)
        pure (offset - delta)
  let discarded = [p | p@(Just _, Nothing) <- zip mbVars mbVarsIx]
  unless (null discarded) $ do
    logSDoc Logger.Warning $ text "Variables discarded due to (offset - delta) underflow: delta =" <+> ppr delta <+> text "," <+> ppr discarded
  let varsIxs = Map.fromList [ (pos :: Int,v) | (pos,Just v) <- zip [0..] mbVarsIx]

  let lookupBCOArgs :: Remote.RemoteExpr ([GHC.StackField] -> [Int] -> IO [HValue])
      lookupBCOArgs = Remote.raw
        "\\unpack fs (ixs :: [Prelude.Int]) -> Prelude.mapM (\\ i -> unpack (Data.Maybe.fromMaybe (Prelude.error (\"Looking up StackField: \" Prelude.++ Prelude.show i)) (fs Data.List.!? i))) ixs :: IO [GHCi.RemoteTypes.HValue]"
        `Remote.app` unpackStackField
  let joinOccs m = Map.elems $ Map.intersectionWith (\(x,y) z -> (x,y,z)) m (Map.fromList $ zip [0..] occs)

  fhvs <- joinOccs <$> do
    withMapElems varsIxs $ \ xs -> withListElems xs $ \ ixs -> do
      res <- Remote.evalIOList $ lookupBCOArgs `Remote.appRef` bcoArgs `Remote.app` (Remote.raw $ show ixs)
      expectRight res

  liftIO $ bindForeignHValues hsc_env fhvs
    where
      withListElems :: Monad m => [(a,b)] -> ([b] -> m [c]) -> m [(a,c)]
      withListElems xs f = do
        let (as,bs) = unzip xs
        bs' <- f bs
        pure $ zip as bs'

      withMapElems :: (Monad m, Ord a) => Map.Map a b -> ([b] -> m [c]) -> m (Map.Map a c)
      withMapElems m f = Map.fromList <$> withListElems (Map.toList m) f

-- Need to be careful not to create extra thunks in the returned `HValue`s, but also avoid forcing the inside of a `Box`.
-- See Note [Forcing debuggee's thunks].
-- TODO: less horrible way to do case expressions in RemoteExpr?
unpackStackField :: Remote.RemoteExpr (GHC.StackField -> IO HValue)
unpackStackField = Remote.raw "\\ x -> case x of (GHC.Internal.Heap.Closures.StackBox (GHC.Internal.Heap.Closures.Box a)) -> GHC.Base.returnIO (GHCi.RemoteTypes.HValue a); (GHC.Internal.Heap.Closures.StackWord w) -> GHC.Base.returnIO (GHCi.RemoteTypes.HValue (Unsafe.Coerce.unsafeCoerce w))"

-- | Modeled after bindLocalsAtBreakpoint
--   Returns new Ids generated from the given ones and OccNames, with refreshed free type variables.
--   The values are bound to the new Ids in the loader state.
bindForeignHValues :: HscEnv -> [(Id, ForeignHValue, GHC.OccName)] -> IO [Id]
bindForeignHValues hsc_env mbVals = do
  let interp = hscInterp hsc_env
  let
    -- Filter out any unboxed ids by changing them to Nothings;
    -- we can't bind these at the prompt

    -- TODO: do we have the same restriction in hdb?
    mbPointers = [x | x@(id',_,_) <- mbVals, isPointer id']

    (ids, hvalues, occs) = unzip3 mbPointers

  new_ids     <- mkNewIds ids occs

  let names  = map GHC.idName new_ids

  let fhvs = hvalues
  Loader.extendLoadedEnv interp
#if MIN_VERSION_ghc(9,14,2)
      modifyHomePackageBytecodeState
#endif
      (zip names fhvs)
  return new_ids
  where
    mkNewIds ids occs = do
      let
        free_tvs = GHC.tyCoVarsOfTypesWellScoped (map idType ids)

      us <- GHC.mkSplitUniqSupply
#if MIN_VERSION_ghc(9,14,2)
              GHC.BcoTag
#else
              'b'
#endif
      let tv_subst     = newTyVars us free_tvs
          tidy_tys = GHC.tidyOpenTypes GHC.emptyTidyEnv $
                      map (GHC.substTy tv_subst . idType) ids
          mkNewId' occ ty id' = mkNewId hsc_env (GHC.occNameFS occ) ty (Just id')
      GHC.zipWith3M mkNewId' occs tidy_tys ids

    mkRuntimeUnkTyVar :: Name -> Kind -> TyVar
    mkRuntimeUnkTyVar name kind = GHC.mkTcTyVar name kind GHC.RuntimeUnk

    newTyVars :: GHC.UniqSupply -> [GHC.TcTyVar] -> GHC.Subst
     -- Similarly, clone the type variables mentioned in the types
     -- we have here, *and* make them all RuntimeUnk tyvars
    newTyVars us tvs = foldl' mk_new_tv GHC.emptySubst (tvs `zip` GHC.uniqsFromSupply us)
    mk_new_tv subst (tv,uniq) = GHC.extendTCvSubstWithClone subst tv new_tv
      where
        new_tv = mkRuntimeUnkTyVar (GHC.setNameUnique (GHC.tyVarName tv) uniq)
                                (GHC.substTy subst (GHC.tyVarKind tv))

    isPointer id' | [rep] <- GHC.typePrimRep (idType id')
                  , GHC.isGcPtrRep rep = True
                  | otherwise          = False

-- | Extends the InteractiveContext with the given Ids, setting up the RTTI information.
--   Assumes the Ids' Names are already known to the Loader.
addIdsToInteractiveContext :: HscEnv -> [Id] -> IO HscEnv
addIdsToInteractiveContext hsc_env final_ids = do
   let
       ictxt0 = hsc_IC hsc_env
       ictxt1 = GHC.extendInteractiveContextWithIds ictxt0 final_ids
   rttiEnvironment hsc_env{ hsc_IC = ictxt1 }

rttiEnvironment :: HscEnv -> IO HscEnv
rttiEnvironment hsc_env0@HscEnv{hsc_IC=ic0} = do
   let tmp_ids = [id' | AnId id' <- GHC.ic_tythings ic0]
       incompletelyTypedIds =
           [id' | id' <- tmp_ids
               , not $ noSkolems id'
               , (GHC.occNameFS . GHC.nameOccName . GHC.idName) id' /= result_fs]
   foldM improveTypes hsc_env0 (map GHC.idName incompletelyTypedIds)
    where
     result_fs :: GHC.FastString
     result_fs = GHC.fsLit "_result"

     noSkolems = GHC.noFreeVarsOfType . idType
     improveTypes hsc_env@HscEnv{hsc_IC=ic} name = do
      let tmp_ids = [id' | AnId id' <- GHC.ic_tythings ic]
      let
          id' = expectJust $ L.find (\i -> GHC.idName i == name) tmp_ids
      if noSkolems id'
         then return hsc_env
         else do
           mb_new_ty <- reconstructType hsc_env 10 id'
           let old_ty = idType id'
           case mb_new_ty of
             Nothing -> return hsc_env
             Just new_ty -> do
              case GHC.improveRTTIType hsc_env old_ty new_ty of
               Nothing -> warnPprTrace True (":print failed to calculate the "
                                             ++ "improvement for a type")
                              (vcat [ text "id" <+> ppr id'
                                    , text "old_ty" <+> GHC.debugPprType old_ty
                                    , text "new_ty" <+> GHC.debugPprType new_ty ]) $
                          return hsc_env
               Just subst -> do
                 let logger = hsc_logger hsc_env
                 GHC.putDumpFileMaybe logger GHC.Opt_D_dump_rtti "RTTI"
                   GHC.FormatText
                   (fsep [text "RTTI Improvement for", ppr id', equals,
                          ppr subst])

                 let ic' = GHC.substInteractiveContext ic subst
                 return hsc_env{hsc_IC=ic'}
