{-# LANGUAGE GADTs #-}

module Cardano.Api.Query.Expr
  ( queryChainBlockNo
  , queryChainPoint
  , queryCurrentEpochState
  , queryCurrentEra
  , queryDebugLedgerState
  , queryEpoch
  , queryEraHistory
  , queryGenesisParameters
  , queryPoolDistribution
  , queryPoolState
  , queryPparams
  , queryProtocolParameters
  , queryProtocolParametersUpdate
  , queryProtocolState
  , queryStakeAddresses
  , queryStakeDelegDeposits
  , queryStakeDistribution
  , queryStakePoolParameters
  , queryStakePools
  , queryStakeSnapshot
  , querySystemStart
  , queryUtxo
  , determineEraExpr
  ) where

import           Cardano.Api.Address
import           Cardano.Api.Block
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.GenesisParameters
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Monad
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.Value

import           Cardano.Slotting.Slot
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus

import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Data.Map (Map)
import           Data.Set (Set)

queryChainBlockNo :: ()
  => LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (WithOrigin BlockNo))
queryChainBlockNo =
  queryExpr QueryChainBlockNo

queryChainPoint :: ()
  => LocalStateQueryExpr block point (QueryInMode CardanoMode) r IO (Either UnsupportedNtcVersionError ChainPoint)
queryChainPoint =
  queryExpr $ QueryChainPoint CardanoMode

queryCurrentEra :: ()
  => LocalStateQueryExpr block point (QueryInMode CardanoMode) r IO (Either UnsupportedNtcVersionError AnyCardanoEra)
queryCurrentEra =
  queryExpr $ QueryCurrentEra CardanoModeIsMultiEra

queryCurrentEpochState :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedCurrentEpochState era)))
queryCurrentEpochState eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryCurrentEpochState

queryEpoch :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch EpochNo))
queryEpoch eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryEpoch

queryDebugLedgerState :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedDebugLedgerState era)))
queryDebugLedgerState eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryDebugLedgerState

queryEraHistory :: ()
  => LocalStateQueryExpr block point (QueryInMode CardanoMode) r IO (Either UnsupportedNtcVersionError (EraHistory CardanoMode))
queryEraHistory =
  queryExpr $ QueryEraHistory CardanoModeIsMultiEra

queryGenesisParameters :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (GenesisParameters era)))
queryGenesisParameters eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryGenesisParameters

queryPoolDistribution :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Maybe (Set PoolId)
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedPoolDistribution era)))
queryPoolDistribution eraInMode sbe mPoolIds =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryPoolDistribution mPoolIds

queryPoolState :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Maybe (Set PoolId)
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedPoolState era)))
queryPoolState eraInMode sbe mPoolIds =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryPoolState mPoolIds

queryPparams :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (ProtocolParameters era)))
queryPparams eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters
{-# DEPRECATED queryPparams "Use queryProtocolParameters instead" #-}

queryProtocolParameters :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (ProtocolParameters era)))
queryProtocolParameters eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters

queryProtocolParametersUpdate :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Map (Hash GenesisKey) ProtocolParametersUpdate)))
queryProtocolParametersUpdate eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryProtocolParametersUpdate

queryProtocolState :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (ProtocolState era)))
queryProtocolState eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryProtocolState

queryStakeAddresses :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Set StakeCredential
  -> NetworkId
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Map StakeAddress Lovelace, Map StakeAddress PoolId)))
queryStakeAddresses eraInMode sbe stakeCredentials networkId =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryStakeAddresses stakeCredentials networkId

queryStakeDelegDeposits :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Set StakeCredential
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either Consensus.EraMismatch (Map StakeCredential Lovelace)))
queryStakeDelegDeposits eraInMode sbe stakeCreds =
  queryExpr $ QueryInEra eraInMode . QueryInShelleyBasedEra sbe $ QueryStakeDelegDeposits stakeCreds

queryStakeDistribution :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Map (Hash StakePoolKey) Rational)))
queryStakeDistribution eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryStakeDistribution

queryStakePoolParameters :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Set PoolId
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Map PoolId StakePoolParameters)))
queryStakePoolParameters eraInMode sbe poolIds =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryStakePoolParameters poolIds

queryStakePools :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Set PoolId)))
queryStakePools eraInMode sbe =
  queryExpr $ QueryInEra eraInMode . QueryInShelleyBasedEra sbe $ QueryStakePools

queryStakeSnapshot :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Maybe (Set PoolId)
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedStakeSnapshots era)))
queryStakeSnapshot eraInMode sbe mPoolIds =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryStakeSnapshot mPoolIds

querySystemStart :: ()
  => LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError SystemStart)
querySystemStart =
  queryExpr QuerySystemStart

queryUtxo :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> QueryUTxOFilter
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (UTxO era)))
queryUtxo eraInMode sbe utxoFilter =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryUTxO utxoFilter

-- | A monad expression that determines what era the node is in.
determineEraExpr :: ()
  => ConsensusModeParams mode
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError AnyCardanoEra)
determineEraExpr cModeParams = runExceptT $
  case consensusModeOnly cModeParams of
    ByronMode -> pure $ AnyCardanoEra ByronEra
    ShelleyMode -> pure $ AnyCardanoEra ShelleyEra
    CardanoMode -> ExceptT queryCurrentEra
