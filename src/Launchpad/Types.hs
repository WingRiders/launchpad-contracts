{-# LANGUAGE BlockArguments #-}

module Launchpad.Types where

import Plutarch
import Plutarch.Api.V1 (PRedeemer)
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Extra.IsData
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Types.Classes
import Plutarch.Util ()
import PlutusLedgerApi.V2
import PlutusTx qualified

-- ScriptHash of the Node Validator
type LaunchpadTokensHolderDatum = ScriptHash

newtype PLaunchpadTokensHolderDatum (s :: S)
  = PLaunchpadTokensHolderDatum (Term s (PAsData PScriptHash))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow)

instance DerivePlutusType PLaunchpadTokensHolderDatum where
  type DPTStrat _ = PlutusTypeNewtype

instance PTryFrom PData PLaunchpadTokensHolderDatum

data LaunchpadTokensHolderFirstRedeemer
  = CancelLaunchpad
  | DelegateToRewardsOrFailure
  | FirstTokensHolderEmergencyWithdrawal
  deriving (Show, Eq, Ord, Generic)

PlutusTx.makeIsDataIndexed
  ''LaunchpadTokensHolderFirstRedeemer
  [ ('CancelLaunchpad, 0)
  , ('DelegateToRewardsOrFailure, 1)
  , ('FirstTokensHolderEmergencyWithdrawal, 2)
  ]
PlutusTx.makeLift ''LaunchpadTokensHolderFirstRedeemer

data PLaunchpadTokensHolderFirstRedeemer (s :: S)
  = PCancelLaunchpad (Term s (PDataRecord '[]))
  | PDelegateToRewardsOrFailure (Term s (PDataRecord '[]))
  | PFirstTokensHolderEmergencyWithdrawal (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PLaunchpadTokensHolderFirstRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PLaunchpadTokensHolderFirstRedeemer where
  type PLifted PLaunchpadTokensHolderFirstRedeemer = LaunchpadTokensHolderFirstRedeemer

deriving via
  (DerivePConstantViaData LaunchpadTokensHolderFirstRedeemer PLaunchpadTokensHolderFirstRedeemer)
  instance
    (PConstantDecl LaunchpadTokensHolderFirstRedeemer)

instance PTryFrom PData PLaunchpadTokensHolderFirstRedeemer

type NodeKey = (BuiltinByteString, Integer)
type PNodeKey = (PBuiltinPair (PAsData PByteString) (PAsData PInteger))

data Node = Node
  { key :: Maybe NodeKey
  , next :: Maybe NodeKey
  , createdTime :: POSIXTime
  , committed :: Integer
  }
  deriving (Show, Eq, Ord, Generic)

PlutusTx.makeIsDataIndexed ''Node [('Node, 0)]
PlutusTx.makeLift ''Node

-- | Represents the datum of a utxo that holds the funds committed to the launchpad by a user.
data PNode (s :: S)
  = PNode
      ( Term
          s
          ( PDataRecord
              [ "key" ':= PMaybeData (PAsData PNodeKey)
              , "next" ':= PMaybeData (PAsData PNodeKey)
              , "createdTime" ':= PPOSIXTime
              , "committed" ':= PInteger
              ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PNode where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PNode where
  type PLifted PNode = Node

deriving via
  (DerivePConstantViaData Node PNode)
  instance
    (PConstantDecl Node)

instance PTryFrom PData PNode

data Tier = Presale | Default
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData) via (EnumIsData Tier)

data PTier (s :: S) = PPresale | PDefault
  deriving stock (Generic, Enum, Bounded)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PTier where
  type DPTStrat _ = PlutusTypeEnumData

instance PTryFrom PData (PAsData PTier)

data NodeRedeemer
  = InsertNode Tier
  | InsertSeparators Integer
  | RemoveCurrentNode
  | RemoveNextNode
  | StartRewardsFold
  | FailLaunchpad
  | DelegateToRewardsFold Integer
  | ReclaimAfterFailure
  | NodeEmergencyWithdrawal
  deriving (Show, Eq, Ord, Generic)

PlutusTx.makeIsDataIndexed
  ''NodeRedeemer
  [ ('InsertNode, 0)
  , ('InsertSeparators, 1)
  , ('RemoveCurrentNode, 2)
  , ('RemoveNextNode, 3)
  , ('StartRewardsFold, 4)
  , ('FailLaunchpad, 5)
  , ('DelegateToRewardsFold, 6)
  , ('ReclaimAfterFailure, 7)
  , ('NodeEmergencyWithdrawal, 8)
  ]

data PNodeRedeemer (s :: S)
  = PInsertNode (Term s (PDataRecord '["tier" ':= PTier]))
  | PInsertSeparators (Term s (PDataRecord '["offset" ':= PInteger]))
  | PRemoveCurrentNode (Term s (PDataRecord '[]))
  | PRemoveNextNode (Term s (PDataRecord '[]))
  | PStartRewardsFold (Term s (PDataRecord '[]))
  | PFailLaunchpad (Term s (PDataRecord '[]))
  | PDelegateToRewardsFold (Term s (PDataRecord '["foldIndex" ':= PInteger]))
  | PReclaimAfterFailure (Term s (PDataRecord '[]))
  | PNodeEmergencyWithdrawal (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PNodeRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PNodeRedeemer where
  type PLifted PNodeRedeemer = NodeRedeemer

deriving via
  (DerivePConstantViaData NodeRedeemer PNodeRedeemer)
  instance
    (PConstantDecl NodeRedeemer)

instance PTryFrom PData PNodeRedeemer

pisRemoveCurrentNodeConstructor :: Term s PRedeemer -> Term s PBool
pisRemoveCurrentNodeConstructor r =
  plet
    (ptryFrom @PNodeRedeemer (pto r) fst)
    ( (flip pmatch)
        \case
          PRemoveCurrentNode _ -> pconstant True
          _ -> pconstant False
    )

pisStartRewardsFoldConstructor :: Term s PRedeemer -> Term s PBool
pisStartRewardsFoldConstructor r =
  plet
    (ptryFrom @PNodeRedeemer (pto r) fst)
    ( (flip pmatch)
        \case
          PStartRewardsFold _ -> pconstant True
          _ -> pconstant False
    )

pisFailLaunchpadConstructor :: Term s PRedeemer -> Term s PBool
pisFailLaunchpadConstructor r =
  plet
    (ptryFrom @PNodeRedeemer (pto r) fst)
    ( (flip pmatch)
        \case
          PFailLaunchpad _ -> pconstant True
          _ -> pconstant False
    )

data CommitFoldDatum = CommitFoldDatum
  { nodeScriptHash :: ScriptHash
  , next :: Maybe NodeKey
  , committed :: Integer
  , cutoffKey :: Maybe NodeKey
  , cutoffTime :: Maybe POSIXTime
  , overcommitted :: Integer
  , nodeCount :: Integer
  , owner :: Address
  }
  deriving (Show, Eq, Ord, Generic)

PlutusTx.makeIsDataIndexed ''CommitFoldDatum [('CommitFoldDatum, 0)]
PlutusTx.makeLift ''CommitFoldDatum

data PCommitFoldDatum (s :: S)
  = PCommitFoldDatum
      ( Term
          s
          ( PDataRecord
              '[ "nodeScriptHash" ':= PScriptHash
               , "next" ':= PMaybeData (PAsData PNodeKey)
               , "committed" ':= PInteger
               , "cutoffKey" ':= PMaybeData (PAsData PNodeKey)
               , "cutoffTime" ':= PMaybeData (PAsData PPOSIXTime)
               , "overcommitted" ':= PInteger
               , "nodeCount" ':= PInteger
               , "owner" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PCommitFoldDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PCommitFoldDatum where
  type PLifted _ = CommitFoldDatum

deriving via
  (DerivePConstantViaData CommitFoldDatum PCommitFoldDatum)
  instance
    (PConstantDecl CommitFoldDatum)

instance PTryFrom PData PCommitFoldDatum

data PCommitFoldScott (s :: S) = PCommitFoldScott
  { nodeScriptHash :: Term s PScriptHash
  , next :: Term s (PMaybeData (PAsData PNodeKey))
  , committed :: Term s PInteger
  , cutoffKey :: Term s (PMaybeData (PAsData PNodeKey))
  , cutoffTime :: Term s (PMaybeData (PAsData PPOSIXTime))
  , overcommitted :: Term s PInteger
  , nodeCount :: Term s PInteger
  , owner :: Term s PAddress
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PCommitFoldScott where
  type DPTStrat _ = PlutusTypeScott

instance ScottConvertible PCommitFoldDatum where
  type ScottOf _ = PCommitFoldScott

  toScott d = pletFields @'["nodeScriptHash", "next", "committed", "cutoffKey", "cutoffTime", "overcommitted", "nodeCount", "owner"] d \dF ->
    pcon
      ( PCommitFoldScott
          dF.nodeScriptHash
          dF.next
          dF.committed
          dF.cutoffKey
          dF.cutoffTime
          dF.overcommitted
          dF.nodeCount
          dF.owner
      )

  fromScott d = pmatch d \dF ->
    pcon
      ( PCommitFoldDatum
          ( pdcons
              # pdata dF.nodeScriptHash
              #$ pdcons
              # pdata dF.next
              #$ pdcons
              # pdata dF.committed
              #$ pdcons
              # pdata dF.cutoffKey
              #$ pdcons
              # pdata dF.cutoffTime
              #$ pdcons
              # pdata dF.overcommitted
              #$ pdcons
              # pdata dF.nodeCount
              #$ pdcons
              # pdata dF.owner
              #$ pdnil
          )
      )

data CommitFoldRedeemer
  = -- | The location of the nodes in the reference inputs
    CommitFold [Integer]
  | DelegateCommitToNode
  | CommitFoldEmergencyWithdrawal
  deriving (Show, Eq, Ord, Generic)

PlutusTx.makeIsDataIndexed
  ''CommitFoldRedeemer
  [ ('CommitFold, 0)
  , ('DelegateCommitToNode, 1)
  , ('CommitFoldEmergencyWithdrawal, 2)
  ]
PlutusTx.makeLift ''CommitFoldRedeemer

data PCommitFoldRedeemer (s :: S)
  = PCommitFold (Term s (PDataRecord '["nodes" ':= PBuiltinList (PAsData PInteger)]))
  | PDelegateCommitToNode (Term s (PDataRecord '[]))
  | PCommitFoldEmergencyWithdrawal (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PCommitFoldRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PCommitFoldRedeemer where
  type PLifted PCommitFoldRedeemer = CommitFoldRedeemer

deriving via
  (DerivePConstantViaData CommitFoldRedeemer PCommitFoldRedeemer)
  instance
    (PConstantDecl CommitFoldRedeemer)

instance PTryFrom PData PCommitFoldRedeemer

data RewardsFoldDatum = RewardsFoldDatum
  { nodeScriptHash :: ScriptHash
  , next :: Maybe NodeKey
  , cutoffKey :: Maybe NodeKey
  , cutoffTime :: Maybe POSIXTime
  , committed :: Integer
  , overcommitted :: Integer
  , commitFoldOwner :: Address
  }
  deriving (Show, Eq, Ord, Generic)

PlutusTx.makeIsDataIndexed ''RewardsFoldDatum [('RewardsFoldDatum, 0)]
PlutusTx.makeLift ''RewardsFoldDatum

data PRewardsFoldDatum (s :: S)
  = PRewardsFoldDatum
      ( Term
          s
          ( PDataRecord
              '[ "nodeScriptHash" ':= PScriptHash
               , "next" ':= PMaybeData (PAsData PNodeKey)
               , "cutoffKey" ':= PMaybeData (PAsData PNodeKey)
               , "cutoffTime" ':= PMaybeData (PAsData PPOSIXTime)
               , "committed" ':= PInteger
               , "overcommitted" ':= PInteger
               , "commitFoldOwner" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PRewardsFoldDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PRewardsFoldDatum where
  type PLifted _ = RewardsFoldDatum

deriving via
  (DerivePConstantViaData RewardsFoldDatum PRewardsFoldDatum)
  instance
    (PConstantDecl RewardsFoldDatum)

instance PTryFrom PData PRewardsFoldDatum

data PRewardsFoldScott (s :: S) = PRewardsFoldScott
  { nodeScriptHash :: Term s PScriptHash
  , next :: Term s (PMaybeData (PAsData PNodeKey))
  , cutoffKey :: Term s (PMaybeData (PAsData PNodeKey))
  , cutoffTime :: Term s (PMaybeData (PAsData PPOSIXTime))
  , committed :: Term s PInteger
  , overcommitted :: Term s PInteger
  , commitFoldOwner :: Term s PAddress
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PRewardsFoldScott where
  type DPTStrat _ = PlutusTypeScott

instance ScottConvertible PRewardsFoldDatum where
  type ScottOf _ = PRewardsFoldScott

  toScott d = pletFields @'["nodeScriptHash", "next", "cutoffKey", "cutoffTime", "committed", "overcommitted", "commitFoldOwner"] d \dF ->
    pcon
      ( PRewardsFoldScott
          dF.nodeScriptHash
          dF.next
          dF.cutoffKey
          dF.cutoffTime
          dF.committed
          dF.overcommitted
          dF.commitFoldOwner
      )

  fromScott d = pmatch d \dF ->
    pcon
      ( PRewardsFoldDatum
          ( pdcons
              # pdata dF.nodeScriptHash
              #$ pdcons
              # pdata dF.next
              #$ pdcons
              # pdata dF.cutoffKey
              #$ pdcons
              # pdata dF.cutoffTime
              #$ pdcons
              # pdata dF.committed
              #$ pdcons
              # pdata dF.overcommitted
              #$ pdcons
              # pdata dF.commitFoldOwner
              #$ pdnil
          )
      )

data RewardsFoldRedeemer
  = -- | Do a fold over the nodes and distribute the rewards.
    -- The commit fold compensation index is ignored in all steps except the last.
    RewardsFold [Integer] [Integer] Integer Integer Integer
  | RewardsFoldEmergencyWithdrawal
  deriving (Show, Eq, Ord, Generic)

PlutusTx.makeIsDataIndexed ''RewardsFoldRedeemer [('RewardsFold, 0), ('RewardsFoldEmergencyWithdrawal, 1)]
PlutusTx.makeLift ''RewardsFoldRedeemer

data PRewardsFoldRedeemer (s :: S)
  = PRewardsFold
      ( Term
          s
          ( PDataRecord
              '[ "inputNodes" ':= PBuiltinList (PAsData PInteger)
               , "outputNodes" ':= PBuiltinList (PAsData PInteger)
               , "commitFoldCompensationIndex" ':= PInteger
               , "inputRewardsFoldIndex" ':= PInteger
               , "inputTokensHolderIndex" ':= PInteger
               ]
          )
      )
  | PRewardsFoldEmergencyWithdrawal (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PRewardsFoldRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PRewardsFoldRedeemer where
  type PLifted PRewardsFoldRedeemer = RewardsFoldRedeemer

deriving via
  (DerivePConstantViaData RewardsFoldRedeemer PRewardsFoldRedeemer)
  instance
    (PConstantDecl RewardsFoldRedeemer)

instance PTryFrom PData PRewardsFoldRedeemer

data RewardsHolderDatum = RewardsHolderDatum
  { owner :: NodeKey
  , projectSymbol :: CurrencySymbol
  , projectToken :: TokenName
  , raisingSymbol :: CurrencySymbol
  , raisingToken :: TokenName
  }
  deriving (Show, Eq, Ord, Generic)

PlutusTx.makeIsDataIndexed ''RewardsHolderDatum [('RewardsHolderDatum, 0)]
PlutusTx.makeLift ''RewardsHolderDatum

-- | Represents the owner of the rewards.
data PRewardsHolderDatum (s :: S)
  = PRewardsHolderDatum
      ( Term
          s
          ( PDataRecord
              [ "owner" ':= PNodeKey
              , "projectSymbol" ':= PCurrencySymbol
              , "projectToken" ':= PTokenName
              , "raisingSymbol" ':= PCurrencySymbol
              , "raisingToken" ':= PTokenName
              ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow, PIsData, PDataFields)

instance DerivePlutusType PRewardsHolderDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PRewardsHolderDatum where
  type PLifted PRewardsHolderDatum = RewardsHolderDatum

deriving via
  (DerivePConstantViaData RewardsHolderDatum PRewardsHolderDatum)
  instance
    (PConstantDecl RewardsHolderDatum)

instance PTryFrom PData PRewardsHolderDatum

data PoolProofDatum = PoolProofDatum
  { projectSymbol :: CurrencySymbol
  , projectToken :: TokenName
  , raisingSymbol :: CurrencySymbol
  , raisingToken :: TokenName
  , -- 0 is WingRidersV2
    -- 1 is SundaeSwapV3
    dex :: Integer
  }
  deriving (Show, Eq, Ord, Generic)

PlutusTx.makeIsDataIndexed ''PoolProofDatum [('PoolProofDatum, 0)]
PlutusTx.makeLift ''PoolProofDatum

data PPoolProofDatum (s :: S)
  = PPoolProofDatum
      ( Term
          s
          ( PDataRecord
              [ "projectSymbol" ':= PCurrencySymbol
              , "projectToken" ':= PTokenName
              , "raisingSymbol" ':= PCurrencySymbol
              , "raisingToken" ':= PTokenName
              , "dex" ':= PInteger
              ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PPoolProofDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PPoolProofDatum where
  type PLifted PPoolProofDatum = PoolProofDatum

deriving via
  (DerivePConstantViaData PoolProofDatum PPoolProofDatum)
  instance
    (PConstantDecl PoolProofDatum)

instance PTryFrom PData PPoolProofDatum

type FailProofDatum = ScriptHash

newtype PFailProofDatum (s :: S)
  = PFailProofDatum (Term s (PAsData PScriptHash))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow)

instance DerivePlutusType PFailProofDatum where
  type DPTStrat _ = PlutusTypeNewtype

instance PTryFrom PData PFailProofDatum
