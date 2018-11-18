{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend where

import Common.Route
import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Control.Monad.Trans.Error (Error (..))
import Data.Align
import Data.Fix
import Data.GADT.Compare
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.These
import Data.Time.Clock.POSIX
import Data.Typeable
import Nix (nixEvalExpr, ErrorCall (..))
import Nix.Atoms
import Nix.Context
import Nix.Effects
import Nix.Eval
import Nix.Exec
import Nix.Expr
import Nix.Normal
import Nix.Options (defaultOptions)
import Nix.Parser
import Nix.Pretty (valueToExpr)
import Nix.Render
import Nix.Scope
import Nix.Value
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core
import Data.Functor.Identity

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      case parseNixText inputText of
        Success a -> do
          p <- el "pre" $ renderP $ cata Free a
          el "div" $ display p
          pure ()
        Failure _ -> text "parse failed"
  }

inputText :: Text
inputText = "let x = 1; in x"

tshow :: Show a => a -> Text
tshow = T.pack . show

type PExpr = Free NExprF (Fix (NValueF Identity))

renderP :: DomBuilder t m => PExpr -> m (Dynamic t PExpr)
renderP = \case
  v@(Pure a) -> do
    text $ tshow a
    pure $ pure v
  f@(Free expr) -> do
    x <- renderExpr renderP expr
    pure $ Free <$> sequence x

{-
--TODO: Use new workflow monad
renderRedex :: (DomBuilder t m, MonadHold t m, PerformEvent t m, MonadIO (Performable m)) => NExpr -> m ()
renderRedex e = do
  text "("
  doReduce <- button "R"
  reduced <- performEvent $ ffor doReduce $ \() -> liftIO (reduce e) >>= pure . \case
    Left err -> text $ "error reducing: " <> tshow err
    Right r -> case r of
      Left () -> text "can't reduce yet"
      Right val -> text "reduced"
  _ <- widgetHold (renderExpr renderRedex $ unFix e) reduced
  text ")"
  pure ()
-}

instance Error SomeException where
  noMsg = SomeException $ ErrorCall "unknown error"
  strMsg = SomeException . ErrorCall

newtype NixDebug m a = NixDebug { unNixDebug :: ExceptT () (ExceptT [SomeException] (ReaderT (Context (NixDebug m) (NThunk (NixDebug m))) m)) a }
  deriving (Functor, Applicative, Monad, MonadPlus, Alternative, MonadCatch, MonadThrow, MonadFix, MonadReader (Context (NixDebug m) (NThunk (NixDebug m))))

instance MonadTrans NixDebug where
  lift = NixDebug . lift . lift . lift

instance Monad m => MonadIntrospect (NixDebug m) where
  recursiveSize _ = pure 0

instance Monad m => MonadExec (NixDebug m) where
  exec' e = fail $ "exec' " <> show e

instance Monad m => MonadInstantiate (NixDebug m) where
  instantiateExpr e = fail $ "instantiateExpr " <> show e

instance Monad m => MonadEnv (NixDebug m) where
  getEnvVar = undefined
  getCurrentSystemOS = undefined
  getCurrentSystemArch = undefined

instance Monad m => MonadHttp (NixDebug m) where
  getURL = undefined

instance Monad m => MonadPutStr (NixDebug m) where
  putStr = undefined

instance Monad m => MonadStore (NixDebug m) where
  addPath' = undefined
  toFile_' = undefined

instance Monad m => MonadFile (NixDebug m) where
  readFile = undefined
  listDirectory = undefined
  getCurrentDirectory = undefined
  canonicalizePath = undefined
  getHomeDirectory = undefined
  doesPathExist = undefined
  doesFileExist = undefined
  doesDirectoryExist = undefined
  getSymbolicLinkStatus = undefined

instance MonadRef m => MonadRef (NixDebug m) where
  type Ref (NixDebug m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (NixDebug m) where
  atomicModifyRef r = lift . atomicModifyRef r

instance Monad m => MonadEffects (NixDebug m)

instance Monad m => Scoped (NThunk (NixDebug m)) (NixDebug m) where
  currentScopes = currentScopesReader
  clearScopes = clearScopesReader @(NixDebug m) @(NThunk (NixDebug m))
  pushScopes = pushScopesReader
  lookupVar = lookupVarReader

runNixDebug :: NixDebug m a -> m (Either [SomeException] (Either () a))
runNixDebug (NixDebug a) = runReaderT (runExceptT (runExceptT a)) (newContext (defaultOptions $ posixSecondsToUTCTime 0))

--TODO: Leverage existing pretty-printer
--TODO: Styling of "reduce" button
--TODO: Small-step reduction
reduce :: (MonadCatch m, MonadFix m, MonadAtomicRef m, GEq (Ref m), Typeable m) => NExpr -> m (Either [SomeException] (Either () (NValue (NixDebug m))))
reduce (Fix e) = runNixDebug $ eval $ NixDebug (throwError ()) <$ e

renderExpr :: DomBuilder t m => (r -> m a) -> NExprF r -> m (NExprF a)
renderExpr r e = case e of
  NConstant a -> do
    text $ atomText a
    pure $ NConstant a
  NSym v -> do
    text v
    pure $ NSym v
  NLet binds body -> do
    text "let "
    binds' <- renderBinds r binds
    text " in "
    body' <- r body
    pure $ NLet binds' body'
  NList l -> do
    text "[ "
    l' <- forM l $ \i -> do
      r i <* text " "
    text "]"
    pure $ NList l'
  NSet b -> NSet <$> renderSet r b
  NRecSet b -> NRecSet <$> (text "rec " *> renderSet r b)
  NSelect s p alt -> do
    s' <- r s
    text "."
    p' <- renderAttrPath r p
    alt' <- forM alt $ \a -> do
      text " or "
      r a
    pure $ NSelect s' p' alt'
  NBinary op a b -> do
    a' <- r a
    text " "
    case op of
      NApp -> blank
      _ -> do
        text $ operatorName $ getBinaryOperator op
        text " "
    b' <- r b
    pure $ NBinary op a' b'

renderAttrPath :: DomBuilder t m => (r -> m a) -> NAttrPath r -> m (NAttrPath a)
renderAttrPath r (h :| t) = do
  h' <- renderKeyName r h
  t' <- forM t $ \n -> do
    text "."
    renderKeyName r n
  pure $ h' :| t'

renderKeyName :: DomBuilder t m => (r -> m a) -> NKeyName r -> m (NKeyName a)
renderKeyName r = \case
  StaticKey n -> do
    text n
    pure $ StaticKey n

renderSet :: DomBuilder t m => (r -> m a) -> [Binding r] -> m [Binding a]
renderSet r b = do
  text "{"
  b' <- renderBinds r b
  text "}"
  pure b'

renderBinds :: DomBuilder t m => (r -> m a) -> [Binding r] -> m [Binding a]
renderBinds r b = forM b $ \i -> do
  bind' <- case i of
    NamedVar p v x -> do
      p' <- renderAttrPath r p
      text " = "
      v' <- r v
      pure $ NamedVar p' v' x
    Inherit mCtx names x -> do
      text "inherit "
      mCtx' <- forM mCtx $ \ctx -> do
        text "("
        ctx' <- r ctx
        text ")"
        pure ctx'
      names' <- forM names $ \i -> case i of
        StaticKey n -> do
          text n
          pure $ StaticKey n
      pure $ Inherit mCtx' names' x
  text ";"
  pure bind'

--newtype EvalWidget t m a = ReaderT (Dynamic t (Context (EvalWidget t m) (NThunk (EvalWidget t m)))) m a
--
--instance Scoped (Dynamic t (NValue (EvalWidget t m))) (EvalWidget t m) where
--  currentScopes = 

{-
renderExpr' :: DomBuilder t m => NExpr -> EvalWidget t m a
renderExpr' = eval
--}
{-
newtype DynLazy t m a = DynLazy { unDynLazy :: Dynamic t (Lazy m a) }

newtype DynamicT t m a = DynamicT { unDynamicT :: Dynamic t (m a) }

instance (Functor m, Reflex t) => Functor (DynamicT t m) where
  fmap f = DynamicT . fmap (fmap f) . unDynamicT

instance (Applicative m, Reflex t) => Applicative (DynamicT t m) where
  pure = DynamicT . pure . pure
  DynamicT f <*> DynamicT x = DynamicT $ liftA2 (<*>) f x
  liftA2 f (DynamicT a) (DynamicT b) = DynamicT $ liftA2 (liftA2 f) a b
  DynamicT a *> DynamicT b = DynamicT $ liftA2 (*>) a b
  DynamicT a <* DynamicT b = DynamicT $ liftA2 (<*) a b

{-
f :: Dynamic t (Int -> (Int, Dynamic t (Int -> (Int, a))))
  -> Dynamic t (Int -> (Int, a))
f d1 = buildDynamic getInitial undefined
  where getInitial = do
          f <- sample $ current d1
          pure $ \n -> f x
-}



type MDyn = m (Behavior t a, Event t (m a))


joinDynThrough :: forall t f a. (Reflex t, Traversable f, Align f) => Dynamic t (f (Dynamic t a)) -> Dynamic t (f a)
joinDynThrough dd =
  let u = alignWith $ \case --TODO: This is inefficient; we shouldn't traverse the whole thing
        This a -> a
        That a -> a
        These _ a -> a
      mrg :: forall a. f (Event t a) -> Event t (f a)
      mrg = undefined -- mergeMap
      b' = pull $ mapM (sample . current) =<< sample (current dd)
      eOuter :: Event t (f a) = pushAlways (mapM (sample . current)) $ updated dd
      eInner :: Event t (f a) = attachWith u b' $ switch $ fmap (mrg . fmap updated) (current dd) --Note: the flip is important because Map.union is left-biased
      readNonFiring :: MonadSample t m => These (Dynamic t a) a -> m a
      readNonFiring = \case
        This d -> sample $ current d
        That a -> return a
        These _ a -> return a
      eBoth :: Event t (f a) = coincidence $ fmap (\m -> pushAlways (mapM readNonFiring . align m) $ mrg $ fmap updated m) (updated dd)
      e' = leftmost [eBoth, eOuter, eInner]
  in unsafeBuildDynamic (sample b') e'

{-
distributeFOverDynPure :: forall t f. Reflex t => f (Dynamic t) -> Dynamic t (f Identity)
distributeFOverDynPure dm =
    let getInitial = DMap.traverseWithKey (\_ -> fmap Identity . sample . current) dm
        edmPre = merge $ DMap.map updated dm
        result = unsafeBuildDynamic getInitial $ flip pushAlways edmPre $ \news -> do
          olds <- sample $ current result
          return $ DMap.unionWithKey (\_ _ new -> new) olds news
    in result
-}

instance (Monad m, Reflex t) => Monad (DynamicT t m) where
  return = pure
--  DynamicT dmx >>= f = do
--    mx <- dmx
    
-- DynamicT $ fmap join $ joinDyn $ fmap (fmap (unDynamicT . f)) x
  (>>) = (*>)
  fail = DynamicT . pure . fail

{-
instance Scoped (Dynamic t (NThunk (DynLazy t m))) (DynLazy t m) where
  currentScopes = fmap currentScopes
-}

--newtype ExprW = ExprW { unExprW :: ReaderT (Dynamic t (Context ...)) ( }
-}
