module PscIde.Server where

import Prelude
import Node.Buffer as Buffer
import Data.Either (either)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Traversable (for)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.Which (which)
import Control.Alt ((<|>))
import Control.Monad.Aff (attempt, Aff, later', makeAff, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, modifyRef)
import Node.ChildProcess (CHILD_PROCESS, ChildProcess, Exit(Normally), onClose, onError, defaultSpawnOptions, spawn, defaultExecOptions, execFile)
import PscIde (NET, quit)

data ServerStartResult
  = Started ChildProcess
  | Closed
  | StartError String

data Attempt
  = NotStarted Int
  | BothStarted Int
  | WStarted Int
  | UStarted Int


type StartingEffects e =
  ( cp ∷ CHILD_PROCESS
  , console ∷ CONSOLE
  , avar ∷ AVAR
  , ref ∷ REF
  | e)

-- | Start a psc-ide server instance
startServer
  ∷ ∀ eff
  . String
  → Int
  → Maybe String
  → Aff (StartingEffects eff) ServerStartResult
startServer exe port projectRoot = do
    startStatus ← liftEff $ newRef {w: Nothing, u: Nothing, c: Nothing}

    forkAff do
      wp ← liftEff $ spawn "cmd" ["/s", "/c", exe <> " -p " <> show port] $ defaultSpawnOptions {cwd = projectRoot}
      liftEff do
        onError wp $ const $ modifyRef startStatus _{w = Nothing}
        onClose wp (\exit → modifyRef startStatus _{c = Just exit})
        modifyRef startStatus _{w = Just wp}

    forkAff do
      up ← liftEff $ spawn exe ["-p", show port ] $ defaultSpawnOptions {cwd = projectRoot}
      liftEff do
        onError up $ const $ modifyRef startStatus _{u = Nothing}
        onClose up (\exit → modifyRef startStatus _{c = Just exit})
        modifyRef startStatus _{u = Just up}


    checkIfOk startStatus $ NotStarted zero

    where
    checkIfOk ∷ Ref {w ∷ Maybe ChildProcess, u ∷ Maybe ChildProcess, c ∷ Maybe Exit} → Attempt → Aff (StartingEffects eff) ServerStartResult
    checkIfOk ref attempts = do
      {w, u, c} ← liftEff $ readRef ref
      case c of
        Just (Normally 0) → pure Closed
        Just (Normally n) → pure $ StartError $ "Error code returned: " <> show n
        Just _ → pure $ StartError "Other close error"
        Nothing →
          case w, u, attempts of
            Nothing, Nothing, NotStarted n | n > 5 → pure $ StartError "Not started after 5 attempts"
            Nothing, Nothing, NotStarted n → later' 300 $ checkIfOk ref $ NotStarted $ n + one
            Nothing, Nothing, _ → checkIfOk ref $ NotStarted zero
            Just w', Just u', BothStarted n | n > 5 → pure $ StartError "Weird environment: both windows and unix processes are running"
            Just w', Just u', BothStarted n → later' 300 $ checkIfOk ref $ BothStarted $ n + one
            Just w', Just u', _ → checkIfOk ref $ BothStarted one
            Just w', _, WStarted n | n > 5 → pure $ Started w'
            Just w', _, WStarted n → later' 300 $ checkIfOk ref $ WStarted $ n + one
            Just w', _, _ → checkIfOk ref $ WStarted one
            _, Just u', UStarted n |n > 5 → pure $ Started u'
            _, Just u', UStarted n → later' 300 $ checkIfOk ref $ UStarted $ n + one
            _, Just u', _ → checkIfOk ref $ UStarted one



-- | Stop a psc-ide server.
stopServer :: forall eff. Int -> Aff (cp :: CHILD_PROCESS, net :: NET | eff) Unit
stopServer port = void $ quit port

data Executable = Executable String (Maybe String)

findBins :: forall eff. String -> Aff (fs :: FS, buffer :: BUFFER, cp :: CHILD_PROCESS | eff) (Array Executable)
findBins exe = do
  bins <- which exe <|> pure []
  for bins \exe -> Executable exe <$> either (const Nothing) Just <$> attempt (getVersion exe)

  where
  getVersion :: forall eff'. String -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS | eff') String
  getVersion exe = makeAff $ \err succ ->
    execFile exe ["--version"] defaultExecOptions \({error, stdout}) -> do
      maybe (Buffer.readString UTF8 0 100 stdout >>= succ) err error
