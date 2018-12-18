{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Frontend where

import Common.Route
import Control.Applicative
import Control.Lens (ifor_)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Trans.Error (Error (..))
import Data.Fix
import Data.GADT.Compare
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Nix (ErrorCall (..))
import Nix.Atoms
import Nix.Context
import Nix.Effects
import Nix.Eval
import Nix.Expr
import Nix.Options (defaultOptions)
import Nix.Parser
import Nix.Render
import Nix.Scope
import Nix.String
import Nix.Thunk (Thunk (..))
import Nix.Value
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core
import Data.Functor.Identity

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "style" ("type" =: "text/css") $ text ".expr:hover { background: rgba(100,200,100,0.2); }"
  , _frontend_body = do
      exprStr <- el "div" $ textAreaElement $ def & textAreaElementConfig_initialValue .~ inputText0
      goNow <- el "div" $ button "Go"
      let go inputText =
            case parseNixText inputText of
              Success a -> do
                _ <- el "pre" $ renderP $ cata Free a
                pure ()
              Failure _ -> text "parse failed"
      _ <- el "div" $ widgetHold (go inputText0) $ go <$> current (value exprStr) <@ goNow
      pure ()
  }

inputText0 :: Text
inputText0 = "false && true"

tshow :: Show a => a -> Text
tshow = T.pack . show

type PExpr = Free NExprF (NValue (NixDebug Identity))

renderValue :: DomBuilder t m => NValue (NixDebug Identity) -> m ()
renderValue v = case _baseValue v of
  NVConstantF a -> text $ atomText a
  NVStrF s -> text $ tshow $ hackyStringIgnoreContext s
  NVSetF vals _ -> do
    text "{ "
    ifor_ vals $ \k t -> do
      text $ tshow k <> " = "
      renderThunk t
      text "; "
    text "}"
  x -> text $ tshow x

renderThunk :: DomBuilder t m => NThunk (NixDebug Identity) -> m ()
renderThunk t = case _baseThunk t of
  Value v -> renderValue v

renderP :: forall t m. (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => PExpr -> m (Dynamic t PExpr)
renderP e = do
  let renderLevel :: PExpr -> m (Event t (NValue (NixDebug Identity)), Dynamic t PExpr)
      renderLevel = \case
        v@(Pure a) -> elAttr "span" ("class" =: "value") $ do
          renderValue a
          pure (never, pure v)
        Free expr -> elAttr "span" ("class" =: ("expr expr-" <> spanType expr)) $ do
          rec doReduce <- switchHold never <=< dyn $ ffor newExpr $ \expr' -> case runIdentity $ reduce expr' of --TODO: Sometimes we can reduce without reducing all the children
                Left err -> elAttr "span" ("title" =: tshow err) $ do
                  text "E"
                  pure never
                Right (Left ()) -> elAttr "span" ("title" =: "Not yet reducible - try reducing children") $ do
                  text "?"
                  pure never
                Right (Right val) -> do
                  fmap (val <$) $ button "R"
              newExpr <- sequence <$> renderExpr renderP expr --TODO: Is there a way we can reuse the rendered DOM?
          pure (doReduce, Free <$> newExpr)
  rec r <- widgetHold (renderLevel e) $ ffor doReduce $ \val -> do
        renderValue val
        pure (never, pure $ Pure val)
      let doReduce = switch $ fst <$> current r
          e' = join $ snd <$> r
  pure e'

data RefIdentity a

instance GEq RefIdentity

instance Monad m => MonadCatch (NixDebug m) where
  catch (NixDebug a) f = NixDebug $ mapExceptT (\b -> catchError b (\se -> case fromException se of { Nothing -> throwError se ; Just e -> runExceptT $ unNixDebug $ f e })) a

instance Monad m => MonadThrow (NixDebug m) where
  throwM = NixDebug . lift . throwError . toException

instance MonadRef Identity where
  type Ref Identity = RefIdentity

--TODO: Leverage existing pretty-printer
--TODO: Styling of "reduce" button
--TODO: Small-step reduction
reduce :: NExprF PExpr -> Identity (Either SomeException (Either () (NValue (NixDebug Identity))))
reduce e = runNixDebug $ eval $ f <$> e
  where f = \case
          Pure a -> pure a
          Free _ -> NixDebug $ throwError ()

instance Error SomeException where
  noMsg = SomeException $ ErrorCall "unknown error"
  strMsg = SomeException . ErrorCall

newtype NixDebug m a = NixDebug { unNixDebug :: ExceptT () (ExceptT SomeException (ReaderT (Context (NixDebug m) (NThunk (NixDebug m))) m)) a }
  deriving (Functor, Applicative, Monad, MonadPlus, Alternative, MonadFix, MonadReader (Context (NixDebug m) (NThunk (NixDebug m))))

instance MonadTrans NixDebug where
  lift = NixDebug . lift . lift . lift

instance Monad m => MonadIntrospect (NixDebug m) where
  recursiveSize _ = pure 0

instance Monad m => MonadExec (NixDebug m) where
  exec' e = notImplemented $ "exec' " <> show e

instance Monad m => MonadInstantiate (NixDebug m) where
  instantiateExpr e = notImplemented $ "instantiateExpr " <> show e

instance Monad m => MonadEnv (NixDebug m) where
  getEnvVar _ = notImplemented "getEnvVar"
  getCurrentSystemOS = notImplemented "getCurrentSystemOS"
  getCurrentSystemArch = notImplemented "getCurrentSystemArch"

instance Monad m => MonadHttp (NixDebug m) where
  getURL _ = notImplemented "getURL"

instance Monad m => MonadPutStr (NixDebug m) where
  putStr _ = notImplemented "putStr"

instance Monad m => MonadStore (NixDebug m) where
  addPath' _ = notImplemented "addPath'"
  toFile_' _ _ = notImplemented "toFile_'"

instance Monad m => MonadFile (NixDebug m) where
  readFile _ = notImplemented "readFile"
  listDirectory _ = notImplemented "listDirectory"
  getCurrentDirectory = notImplemented "getCurrentDirectory"
  canonicalizePath _ = notImplemented "canonicalizePath"
  getHomeDirectory = notImplemented "getHomeDirectory"
  doesPathExist _ = notImplemented "doesPathExist"
  doesFileExist _ = notImplemented "doesFileExist"
  doesDirectoryExist _ = notImplemented "doesDirectoryExist"
  getSymbolicLinkStatus _ = notImplemented "getSymbolicLinkStatus"

instance Monad m => MonadRef (NixDebug m) where
  type Ref (NixDebug m) = Ref m
  newRef _ = notImplemented "newRef"
  readRef _ = notImplemented "readRef"
  writeRef _ _ = notImplemented "writeRef"

instance Monad m => MonadAtomicRef (NixDebug m) where
  atomicModifyRef _ _ = notImplemented "atomicModifyRef"

instance Monad m => MonadEffects (NixDebug m) where
  makeAbsolutePath _ = notImplemented "makeAbsolutePath"
  findEnvPath _ = notImplemented "findEnvPath"
  findPath _ _ = notImplemented "findPath"

  importPath _ = notImplemented "importPath"
  pathToDefaultNix _ = notImplemented "pathToDefaultNix"

  derivationStrict _ = notImplemented "derivationStrict"

  traceEffect _ = notImplemented "traceEffect"

instance Monad m => Scoped (NThunk (NixDebug m)) (NixDebug m) where
  currentScopes = currentScopesReader
  clearScopes = clearScopesReader @(NixDebug m) @(NThunk (NixDebug m))
  pushScopes = pushScopesReader
  lookupVar = lookupVarReader

notImplemented :: Monad m => String -> NixDebug m a
notImplemented = NixDebug . lift . throwError . SomeException . ErrorCall

runNixDebug :: NixDebug m a -> m (Either SomeException (Either () a))
runNixDebug (NixDebug a) = runReaderT (runExceptT (runExceptT a)) (newContext (defaultOptions $ posixSecondsToUTCTime 0))

spanType :: NExprF a -> Text
spanType = \case
  NConstant _ -> "constant"
  NSym _ -> "sym"
  NLet _ _ -> "let"
  NList _ -> "list"
  NSet _ -> "set"
  NRecSet _ -> "rec-set"
  NSelect _ _ _ -> "select"
  NBinary _ _ _ -> "binary"
  NStr _ -> "str"
  NLiteralPath _ -> "literal-path"
  NEnvPath _ -> "env-path"
  NUnary _ _ -> "unary"
  NHasAttr _ _ -> "has-attr"
  NAbs _ _ -> "abs"
  NIf _ _ _ -> "if"
  NWith _ _ -> "with"
  NAssert _ _ -> "assert"

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
  NStr s -> NStr <$> case s of
    DoubleQuoted l -> do
      text "\""
      l' <- forM l $ \case
        Plain v -> do
          text v
          pure $ Plain v
      text "\""
      pure $ DoubleQuoted l'
  _ -> do
    text $ "<" <> spanType e <> ">"
    result <- traverse r e
    text $ "</" <> spanType e <> ">"
    pure result

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
