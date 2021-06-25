module ContractHome.State
  ( dummyState
  , mkInitialState
  , handleAction
  , partitionContracts
  ) where

import Prelude
import Contract.State (isContractClosed)
import Contract.State (mkInitialState) as Contract
import Contract.Types (State) as Contract
import ContractHome.Lenses (_selectedContractIndex, _status)
import ContractHome.Types (Action(..), ContractStatus(..), PartitionedContracts, State)
import Data.Array (partition)
import Data.Lens (assign)
import Data.Map (Map, lookup, mapMaybeWithKey, toUnfoldableUnordered)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (snd)
import Halogen (HalogenM)
import MainFrame.Types (ChildSlots, Msg)
import Marlowe.PAB (ContractHistory, PlutusAppId)
import Marlowe.Semantics (Slot)
import WalletData.State (defaultWalletDetails)
import WalletData.Types (WalletDetails)

-- see note [dummyState] in MainFrame.State
dummyState :: State
dummyState = mkInitialState defaultWalletDetails zero mempty mempty

mkInitialState :: WalletDetails -> Slot -> Map PlutusAppId ContractHistory -> Map PlutusAppId String -> State
mkInitialState walletDetails currentSlot contracts contractNicknames =
  let
    mkInitialContractState followerAppId contractHistory =
      let
        nickname = fromMaybe mempty $ lookup followerAppId contractNicknames
      in
        Contract.mkInitialState walletDetails currentSlot followerAppId nickname contractHistory
  in
    { status: Running
    , contracts: mapMaybeWithKey mkInitialContractState contracts
    , selectedContractIndex: Nothing
    }

handleAction ::
  forall m.
  Action -> HalogenM State Action ChildSlots Msg m Unit
handleAction OpenTemplateLibraryCard = pure unit -- handled in Play.State

handleAction (SelectView view) = assign _status view

handleAction (OpenContract ix) = assign _selectedContractIndex $ Just ix

partitionContracts :: Map PlutusAppId Contract.State -> PartitionedContracts
partitionContracts contracts =
  toUnfoldableUnordered contracts
    # map snd
    # partition isContractClosed
    # \{ no, yes } -> { completed: yes, running: no }
