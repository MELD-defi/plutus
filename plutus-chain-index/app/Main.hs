{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import qualified Config                              as Config
import           Control.Concurrent.MVar             (MVar, putMVar, takeMVar)
import qualified Control.Concurrent.STM              as STM
import           Control.Monad.Freer                 (Eff, interpret, runM)
import           Control.Monad.Freer.Error           (Error, runError)
import           Control.Monad.Freer.Extras          (LogMsg)
import           Control.Monad.Freer.State           (State, runState)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Foldable                       (for_)
import           Data.Function                       ((&))
import           Data.Text.Prettyprint.Doc           (Pretty (..))
import           Data.Yaml                           (decodeFileThrow)
import           Options.Applicative                 (execParser)

import qualified Cardano.BM.Configuration.Model      as CM
import           Cardano.BM.Setup                    (setupTrace_)
import           Cardano.BM.Trace                    (Trace, logError)

import           Cardano.Protocol.Socket.Client      (ChainSyncEvent (..))
import           CommandLine                         (AppConfig (..), Command (..), cmdWithHelpParser)
import           Config                              (ChainIndexConfig)
import           Ledger                              (Slot (..))
import           Logging                             (defaultConfig, loadConfig)
import           Plutus.ChainIndex.Compatibility     (fromCardanoBlock, fromCardanoTip)
import           Plutus.ChainIndex.Effects           (ChainIndexControlEffect (..), ChainIndexQueryEffect (..),
                                                      appendBlock, rollback)
import           Plutus.ChainIndex.Emulator.Handlers (ChainIndexEmulatorState (..), ChainIndexError (..),
                                                      ChainIndexLog (..), handleControl, handleQuery)
import qualified Plutus.ChainIndex.Server            as Server
import           Plutus.Monitoring.Util              (handleLogMsgTrace)

type ChainIndexEffects m
  = '[ ChainIndexControlEffect
     , ChainIndexQueryEffect
     , State ChainIndexEmulatorState
     , Error ChainIndexError
     , LogMsg ChainIndexLog
     , m
     ]

runChainIndex
  :: Trace IO ChainIndexLog
  -> MVar ChainIndexEmulatorState
  -> Eff (ChainIndexEffects IO) a
  -> IO ()
runChainIndex trace emulatorState effect = do
  oldEmulatorState <- liftIO $ takeMVar emulatorState
  result <- liftIO
    $ interpret handleControl effect
    & interpret handleQuery
    & runState oldEmulatorState
    & runError
    & interpret (handleLogMsgTrace trace)
    & runM
  case result of
    Left  err ->
      logError trace (Err err)
    Right (_, newState) -> do
      liftIO $ putMVar emulatorState newState
      pure ()

chainSyncHandler
  :: Trace IO ChainIndexLog
  -> MVar ChainIndexEmulatorState
  -> ChainSyncEvent
  -> Slot
  -> IO ()
chainSyncHandler trace mState
  (RollForward block tip) _ = do
    let ciBlock = fromCardanoBlock block
    case ciBlock of
      Left err    ->
        logError trace (ConversionFailed err)
      Right block ->
        runChainIndex trace mState $ appendBlock (fromCardanoTip tip) block
chainSyncHandler trace mState
  (RollBackward _ tip) _ =
    -- Do we really want to pass the tip of the new blockchain to the
    -- rollback function (rather than the divergence point)?
    runChainIndex trace mState $ rollback (fromCardanoTip tip)
-- On resume we do nothing, for now.
chainSyncHandler _ _ (Resume _) _ = pure ()

main :: IO ()
main = do
  -- Parse comand line arguments.
  cmdConfig@AppConfig{acLogConfigPath, acConfigPath, acMinLogLevel, acCommand} <- execParser cmdWithHelpParser

  -- Initialise logging
  logConfig <- maybe defaultConfig loadConfig acLogConfigPath
  for_ acMinLogLevel $ \ll -> CM.setMinSeverity logConfig ll
  (trace :: Trace IO ChainIndexLog, _) <- setupTrace_ logConfig "chain-index"

  -- Reading configuration file
  config <- case acConfigPath of
              Nothing -> pure Config.defaultConfig
              Just p  -> decodeFileThrow @IO @ChainIndexConfig p

  putStrLn "Command line config:"
  print cmdConfig

  putStrLn "Configuration file:"
  print (pretty config)

  appState <- STM.newTVarIO mempty

  case acCommand of
    StartChainIndex{} -> do
      putStrLn $ "Starting webserver on port " <> show (Config.cicPort config)
      Server.serveChainIndexQueryServer (Config.cicPort config) appState
    _ -> pure ()
