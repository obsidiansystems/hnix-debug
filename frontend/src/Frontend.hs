{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Common.Route
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Fix
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Data.Functor.Identity
import Nix (nixEvalExpr)
import Nix.Atoms
import Nix.Context
import Nix.Eval
import Nix.Exec
import Nix.Expr
import Nix.Normal
import Nix.Options (defaultOptions)
import Nix.Parser
import Nix.Pretty (valueToExpr)
import Nix.Value
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Reflex.Dom.Core

import System.IO.Unsafe

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      case parseNixText inputText of
        Success a -> el "pre" $ renderRedex a
        Failure _ -> text "parse failed"
  }

inputText :: Text
inputText = "({x.y = 5;}.x).y"

--TODO: Use new workflow monad
renderRedex :: (DomBuilder t m, MonadHold t m) => NExpr -> m ()
renderRedex e = do
  text "("
  doReduce <- button "R"
  _ <- widgetHold (renderExpr renderRedex $ unFix e) $ renderExpr renderRedex (unFix $ reduce e) <$ doReduce
  text ")"
  pure ()

--TODO: Eliminate unsafePerformIO
--TODO: Leverage existing pretty-printer
--TODO: Styling of "reduce" button
--TODO: Small-step reduction
reduce :: NExpr -> NExpr
reduce e = valueToExpr $ unsafePerformIO $ evalStateT (runReaderT (runLazy $ normalForm =<< nixEvalExpr Nothing e) (newContext (defaultOptions $ posixSecondsToUTCTime 0))) mempty

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
  NSelect s p alt -> do
    r s
    text "."
    renderAttrPath r p

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
