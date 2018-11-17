{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend where

import Common.Route
import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Control.Monad.Trans.Error
import Data.GADT.Compare
import Data.Fix
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Data.Typeable
import Nix (nixEvalExpr, ErrorCall (..))
import Nix.Atoms
import Nix.Context
import Nix.Effects
import Nix.Exec
import Nix.Expr
import Nix.Normal
import Nix.Options (defaultOptions)
import Nix.Parser
import Nix.Pretty (valueToExpr)
import Nix.Render
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      case parseNixText inputText of
        Success a -> el "pre" $ renderRedex a
        Failure _ -> text "parse failed"
  }

inputText :: Text
inputText = "1 + 2 * 3"

tshow :: Show a => a -> Text
tshow = T.pack . show

--TODO: Use new workflow monad
renderRedex :: (DomBuilder t m, MonadHold t m, PerformEvent t m, MonadIO (Performable m)) => NExpr -> m ()
renderRedex e = do
  text "("
  doReduce <- button "R"
  reduced <- performEvent $ ffor doReduce $ \() -> liftIO (reduce e) >>= pure . \case
    Left err -> text $ "error reducing: " <> tshow err
    Right expr -> renderExpr renderRedex $ unFix expr
  _ <- widgetHold (renderExpr renderRedex $ unFix e) reduced
  text ")"
  pure ()

instance Error SomeException where
  noMsg = SomeException $ ErrorCall "unknown error"
  strMsg = SomeException . ErrorCall

newtype NixDebug m a = NixDebug { unNixDebug :: ExceptT [SomeException] m a }
  deriving (Functor, Applicative, Monad, MonadPlus, Alternative, MonadCatch, MonadThrow, MonadFix)

instance MonadTrans NixDebug where
  lift = NixDebug . lift

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

runNixDebug :: MonadIO m => NixDebug m a -> m (Either [SomeException] a)
runNixDebug (NixDebug a) = runExceptT a

--TODO: Leverage existing pretty-printer
--TODO: Styling of "reduce" button
--TODO: Small-step reduction
reduce :: (MonadIO m, MonadCatch m, MonadFix m, MonadAtomicRef m, GEq (Ref m), Typeable m) => NExpr -> m (Either [SomeException] NExpr)
reduce e = fmap (fmap valueToExpr) $ runNixDebug $ evalStateT (runReaderT (runLazy $ normalForm =<< nixEvalExpr Nothing e) (newContext (defaultOptions $ posixSecondsToUTCTime 0))) mempty

renderExpr :: DomBuilder t m => (r -> m ()) -> NExprF r -> m ()
renderExpr r e = case e of
  NConstant a -> text $ atomText a
  NSym v -> text v
  NList l -> do
    text "["
    el "ul" $ forM_ l $ el "li" . r
    text "]"
  NSet b -> renderSet r b
  NRecSet b -> text "rec " >> renderSet r b
  NSelect s p _ -> do
    r s
    text "."
    renderAttrPath r p
  NBinary op a b -> do
    r a
    text " "
    case op of
      NApp -> blank
      _ -> do
        text $ operatorName $ getBinaryOperator op
        text " "
    r b

renderAttrPath :: DomBuilder t m => (r -> m ()) -> NAttrPath r -> m ()
renderAttrPath r (h :| t) = do
  renderKeyName r h
  forM_ t $ \n -> do
    text "."
    renderKeyName r n

renderKeyName :: DomBuilder t m => (r -> m ()) -> NKeyName r -> m ()
renderKeyName r = \case
  StaticKey n -> text n

renderSet :: DomBuilder t m => (r -> m ()) -> [Binding r] -> m ()
renderSet r b = do
  text "{"
  el "ul" $ forM_ b $ \i -> el "li" $ do
    case i of
      NamedVar p v _ -> do
        renderAttrPath r p
        text " = "
        r v
      Inherit mCtx names _ -> do
        text "inherit "
        forM_ mCtx $ \ctx -> do
          text "("
          r ctx
          text ")"
        el "ul" $ forM_ names $ \i -> el "li" $ case i of
          StaticKey n -> text n
    text ";"
  text "}"
