# Launchpad

## Architecture Overview

The launchpad allows launching tokens into circulation with the help of the WingRiders platform.

The tokens are locked into a holder script and an on-chain linked list is created, to which users can add their buy requests.

After some specified time elapses, the sales phase is over, and the linked list is folded over a few times to distribute the rewards to the users. The distribution is configurable by the owner of the launchpad.

The remaining project tokens and the collected raised tokens are used to create a WingRiders and/or a Sundae pool, and the resulting liquidity tokens are locked into a vesting smart contract.

Project owners choose a `maxCommitment` which specifies their fundraising goal for the launch.
Users commit the raising tokens until `maxCommitment` is reached, at which point the UI hides the option to commit tokens to that launch. After the sales phase ends successfully, two folds are performed on-chain on the linked lists with the commitments, one fold to count the committed tokens from the nodes and determine the rewards cutoff if any, and a second fold to distribute the rewards and collect the commitments. Users before the cutoff get the full rewards proportionally to their commitment, users after cutoff get their commitments returned.

Initialization of the launch involves creating a token holder utxo with its validity token and a head node of the linked list with its validity token. The transaction has to spend a utxo which TxOutRef parameterizes the whole project.

The launch owner has to lock in a specific amount of collateral Ada which is used for utxo min Ada and in case the min commitment of the launch is not reached.

The launchpad supports a tiered whitelisting with a presale tier. It is a way to split the users in different groups based on whether they have a specific token or not; the policy id of the tier token is configurable by the launch owner. It is possible to specify min/max commitment and the start commitment time for the presale tier, and min commitment and start time for the default tier.

Here is the full configuration that is needed for the launch:
```haskell
data LaunchpadConfig = LaunchpadConfig
  { owner :: Address
  -- ^ The owner of the launch, supplies the project tokens
  , splitBps :: Integer
  -- ^ A value between 0 and 10_000, specifies a split of liquidity between Sundae and Wr pools
  -- 0 means everything goes to Sundae
  -- 10_000 means everything goes to Wr
  , wrPoolValidatorHash :: ScriptHash
  -- ^ The script hash of the Wr V2 Constant Product pool
  , wrFactoryValidatorHash :: ScriptHash
  -- ^ The script hash of the Wr V2 factory
  , wrPoolCurrencySymbol :: CurrencySymbol
  -- ^ The currency symbol of the Wr V2 pool policy
  , sundaePoolScriptHash :: ScriptHash
  -- ^ The script hash of the Sundae V3 pool
  , sundaeFeeTolerance :: Integer
  -- ^ The max amount of ada it is tolerable to pay to create a Sundae pool
  -- Note that the actual amount is hopefully less and is controlled by the Sundae settings utxo.
  -- In case the settings specify a value above the tolerance, no Sundae pool is created.
  , sundaeSettingsCurrencySymbol :: CurrencySymbol
  -- ^ The currency symbol of the Sundae settings NFT.
  , startTime :: POSIXTime
  -- ^ The start time must be set to the lowest of the tiers start times.
  , contributionEndTime :: POSIXTime
  -- ^ The time after which users no longer can contribute to the launch.
  , withdrawalEndTime :: POSIXTime
  -- ^ The time after which users no longer can withdraw from the launch.
  -- the withdrawalEndTime must be after the contributionEndTime
  , projectToken :: AssetClass
  -- ^ The asset that is being launched
  , raisingToken :: AssetClass
  -- ^ The assets that is being raised
  , projectMinCommitment :: Integer
  -- ^ The min possible amount of tokens the launchpad can raise.
  -- In case less raised tokens are collected, the launch is considered failed and tokens are returned back.
  , projectMaxCommitment :: Integer
  -- ^ The maximum amount of tokens the launchpad can raise.
  -- Can be set to max int64 value to essentially remove the cap.
  , totalTokens :: Integer
  -- ^ The total number of the project tokens committed to the launchpad.
  , tokensToDistribute :: Integer
  -- ^ The number of the project tokens to distribute among the launchpad users.
  , raisedTokensPoolPartPercentage :: Integer
  -- ^ The percentage of the raised tokens to place into the pool.
  , daoFeeNumerator :: Integer
  -- ^ Controls the dao fee collected in raised tokens
  , daoFeeDenominator :: Integer
  -- ^ Controls the dao fee collected in raised tokens
  , daoFeeReceiver :: Address
  -- ^ Controls the address where the dao fee is sent
  , daoAdmin :: PubKeyHash
  -- ^ Controls the pub key hash of a dao admin.
  -- This signer can add separator nodes (the launch owner can do that as well).
  , collateral :: Integer
  -- ^ How much collateral (in Lovelace) is locked into the launch
  -- Must be at least 2 ada per used DEX plus 2 ada for the dao fee utxo
  -- The rest is returned if the launch is succesful.
  -- If the launch is failed (not cancelled), the collateral is split between the commit fold owner and the dao fee receiver
  , starter :: TxOutRef
  -- ^ The tx out ref of the utxo that has to be spent to uniquely identify a launch
  , vestingPeriodDuration :: POSIXTime
  -- ^ Configures the duration period of the vesting utxo which holds the owner's shares
  , vestingPeriodDurationToFirstUnlock :: POSIXTime
  -- ^ Configures the duration period to first unlock of the vesting utxo which holds the owner's shares
  , vestingPeriodInstallments :: Integer
  -- ^ Configures the number of installments of the vesting utxo which holds the owner's shares
  , vestingPeriodStart :: POSIXTime
  -- ^ Configures the start of the vesting utxo which holds the owner's shares
  , vestingValidatorHash :: ScriptHash
  -- ^ Configures the validator hash of the vesting utxo which holds the owner's shares
  , presaleTierCs :: CurrencySymbol
  -- ^ The currency symbol of the presale tier token
  -- Note that the token name is not checked.
  -- That allows using NFTs on the same policy as presale tokens.
  , presaleTierStartTime :: POSIXTime
  -- ^ The commitment start time of the presale tier
  , defaultStartTime :: POSIXTime
  -- ^ The commitment start time of the default tier
  , presaleTierMinCommitment :: Integer
  -- ^ The min user commitment of the presale tier
  , defaultTierMinCommitment :: Integer
  -- ^ The min user commitment of the default tier
  , presaleTierMaxCommitment :: Integer
  -- ^ The max user commitment of the presale tier
  , defaultTierMaxCommitment :: Integer
  -- ^ The max user commitment of the  tier
  , nodeAda :: Integer
  -- ^ The amount of ada a commitment node must hold.
  -- This ada is used up to compensate the folds and to create a user rewards holder.
  , commitFoldFeeAda :: Integer
  -- ^ The amount of ada the commit fold gets per each folded node.
  , oilAda :: Integer
  -- ^ The min amount of ada various utxos are expected to held.
  -- This includes the rewards holder, dao fee, and final project tokens holder.
  }
  deriving (Show, Eq, Ord)
```

## Contracts

Various smart contracts must be provided with the parts they need to function. Contracts are organized in a way that it's possible to apply the data-encoded configuration options to the compiled contracts to derive a launch-specific suite of contracts.

### First project tokens holder

Has a validator and a minting policy in separate contracts.

The minting policy has the following configuration, the redeemer is ignored:
```haskell
data TokensHolderPolicyConfig = TokensHolderPolicyConfig
  { owner :: PubKeyHash
  , startTime :: POSIXTime
  , totalTokens :: Integer
  , projectSymbol :: CurrencySymbol
  , projectToken :: TokenName
  , collateral :: Integer
  , starter :: TxOutRef
  , nodeSymbol :: CurrencySymbol
  }
```

The minting policy is responsible for ensuring the node token is minted and for checking the project tokens holder utxo is created with the correct value and datum. The policy allows burning the token at any time, and it's the validators's responsibility to control when burning is allowed.

The validator has the following configuration, datum and redeemer:
```haskell
data TokensHolderFirstConfig = TokensHolderFirstConfig
  { owner :: Address
  , startTime :: POSIXTime
  , projectTokensHolderSymbol :: CurrencySymbol
  , starter :: TxOutRef
  , withdrawalEndTime :: POSIXTime
  , daoAdmin :: PubKeyHash
  }
  
data LaunchpadTokensHolderFirstRedeemer
  = CancelLaunchpad
  | DelegateToRewardsOrFailure
  | FirstTokensHolderEmergencyWithdrawal

-- ScriptHash of the Node Validator
type LaunchpadTokensHolderDatum = ScriptHash
```

The validator is responsible for checking 3 types of transactions:
- the rewards fold
- the launchpad cancellation
- the emergency withdrawal

The rewards fold delegates the checks to the node validator by identifying an input node utxo with a proper redeemer. It is used during the rewards fold phase.

The launchpad cancellation has to ensure the project tokens holder token is burned, the launch contribution hasn't started yet and the launch owner has signed the transaction. It is used to cancel the launch before its start.

The emergency withdrawal is there for the case something has gone terribly wrong and the funds need to be reclaimed. The project tokens holder token must be burned, the transaction is signed by the DAO admin, and sufficient time (30 days) has passed since the start of the contribution phase. The DAO admin has to sign the transaction since the project tokens holder might hold both owner and user funds at this point, and the DAO has to return the tokens back to the rightful owners.

### Node (User contributions in a linked list)

Has a validator and a minting policy in separate contracts.

The minting policy has the following configuration, the redeemer is ignored:
```haskell
data NodePolicyConfig = NodePolicyConfig
  { starter :: TxOutRef
  , owner :: PubKeyHash
  , nodeAda :: Integer
  }
```

The minting policy supports the following types of transactions:
- minting the head node
- adding a new element
- adding one or more separator nodes
- removing an element from the list

When the head node is minted, the policy validates the starter utxo is spent, the launch owner signs the transaction, and that the initial head node datum and value are correct.

When a new node or separators are inserted, the minting policy checks the number of mints and delegates to the node validator.

When a node is removed, the minting policy delegates to the node validator.

The validator has the following configuration, datum and redeemers:
```haskell
data NodeConfig = NodeConfig
  { starter :: TxOutRef
  , nodeSymbol :: CurrencySymbol
  , rewardsFoldSymbol :: CurrencySymbol
  , rewardsFoldValidatorHash :: ScriptHash
  , commitFoldSymbol :: CurrencySymbol
  , commitFoldValidatorHash :: ScriptHash
  , tokensHolderSymbol :: CurrencySymbol
  , tokensHolderValidatorHash :: ScriptHash
  , failProofSymbol :: CurrencySymbol
  , failProofValidatorHash :: ScriptHash
  , presaleTierCs :: CurrencySymbol
  , presaleTierMinCommitment :: Integer
  , presaleTierMaxCommitment :: Integer
  , presaleTierStartTime :: POSIXTime
  , defaultTierMinCommitment :: Integer
  , defaultTierMaxCommitment :: Integer
  , defaultStartTime :: POSIXTime
  , startTime :: POSIXTime
  , contributionEndTime :: POSIXTime
  , withdrawalEndTime :: POSIXTime
  , projectMinCommitment :: Integer
  , projectMaxCommitment :: Integer
  , totalTokens :: Integer
  , projectSymbol :: CurrencySymbol
  , projectToken :: TokenName
  , raisingSymbol :: CurrencySymbol
  , raisingToken :: TokenName
  , owner :: Address
  , daoAdmin :: PubKeyHash
  , daoFeeReceiver :: Address
  , collateral :: Integer
  , nodeAda :: Integer
  , oilAda :: Integer
  , commitFoldFeeAda :: Integer
  }

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

type NodeKey = (BuiltinByteString, Integer)
data Node = Node
  { key :: Maybe NodeKey
  , next :: Maybe NodeKey
  , createdTime :: POSIXTime
  , committed :: Integer
  }
```

The validator supports the following types of transactions:
- user node insertion
- separator nodes insertion
- removing a user node
- starting the rewards fold
- rewards folding
- failing the launch
- reclamation in a failed launch
- emergency withdrawal

During a node insertion there is one node on the inputs and two on the outputs. The validator checks if the values, addresses and datums of the resulting nodes are correct, if the owner of the new node signed the transaction, and if the contribution happens during the contribution phase.

During separator nodes insertion there is one input node and >=1 new separator nodes. The validator checks if the values, addresses and datums of the resulting nodes are correct (separator nodes are marked explicitly), if the launch owner or the DAO admin signed the transaction and if the launch has not started yet.

During removal of a user node, there are two input nodes and 1 output node. One of the input nodes is removed and gets the RemoveCurrentNode redeemer, the second one has to be updated to point to the correct next node and gets the RemoveNextNode redeemer. The next node removal case just checks if there is a different node with the correct redeemer and delegates the checks to it. The current node removal validation ensures the owner of the removed node signs the transaction, that its parent is correctly recreated with the new next link, and that the current time is before the end of the withdrawal period.

When the rewards fold is started, the head node must be spent, the commit fold must be validated to be correct, and the rewards fold must be created. The node validator ensures all of that.

During the rewards fold the nodes can be processed by the rewards fold utxo. The validation is delegated to the rewards fold validator in such a case.

In case the launchpad fails, the head node must validate the commit fold's data that establishes the fail. It checks whether the commit fold is correct, if all the correct tokens are burned, whether the fail proof utxo is created and the DAO and commit fold owner are compensated.

Once the fail proof is established, user and separator nodes can be reclaimed by referencing the fail proof. The validator checks its validity, if the owners of the nodes signed the transaction, and whether the correct number of node tokens were burned.

In case something goes terribly wrong, the node validator supports emergency withdrawal. It can be activate once 30 days have passed since the start of the launch and allows reclaiming the locked tokens by the owner of the node.

### Commit fold

Has a validator and a minting policy in separate contracts. 

The minting policy has the following configuration, the redeemer is ignored:
```haskell
data CommitFoldPolicyConfig = CommitFoldPolicyConfig
  { starter :: TxOutRef
  , contributionEndTime :: POSIXTime
  , withdrawalEndTime :: POSIXTime
  , nodeSymbol :: CurrencySymbol
  }
```

The minting policy is responsible for ensuring the initialization of the fold by validating the correct mint, correct head node in the reference inputs, and correct newly create commit fold utxo. The policy allows burning the token at any time, and it's the validators's responsibility to control when burning is allowed.

The validator has the following configuration:
```haskell
data CommitFoldConfig = CommitFoldConfig
  { starter :: TxOutRef
  , commitFoldSymbol :: CurrencySymbol
  , nodeSymbol :: CurrencySymbol
  , withdrawalEndTime :: POSIXTime
  , daoAdmin :: PubKeyHash
  }

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

data CommitFoldRedeemer
  = -- | The location of the nodes in the reference inputs
    CommitFold [Integer]
  | DelegateCommitToNode
  | CommitFoldEmergencyWithdrawal
```

There are thres types of transactions:
- commit fold over a subset of nodes
- spending the commit fold to start a rewards fold or fail the launch
- emergency withdrawal

When a commit fold is performed, the validator ensure the correct subset of a nodes linked list is referenced and ensure the recreated commit fold is correct with regards to the processed nodes and their commitments.

When a commit fold is finished, it can be spent to create a rewards fold or fail the launch depending on the counted up commitment. The validator delegates the checks to a node validator with a correct redeemer.

In case something goes terribly wrong, the commit fold can be spent after 30 days have passed since the start of the launch. The commit fold owner signs the transaction.

### Rewards fold

Has a validator and a minting policy in separate contracts.

The minting policy has the following configuration, the redeemer is ignored:
```haskell
type RewardsFoldPolicyConfig = TxOutRef
```

The policy ensures a node utxo with the correct redeemer ensure the rewards fold is started correctly. The token can always be burned and it's up to the validator to ensure the correct conditions for that.

The validator has the following configuration:
```haskell
data RewardsFoldConfig = RewardsFoldConfig
  { starter :: TxOutRef
  , owner :: Address
  , nodeSymbol :: CurrencySymbol
  , rewardsFoldPolicy :: CurrencySymbol
  , rewardsHolderValidatorHash :: ScriptHash
  , finalProjectTokensHolderValidatorHash :: ScriptHash
  , firstProjectTokensHolderValidatorHash :: ScriptHash
  , projectTokensHolderPolicy :: CurrencySymbol
  , projectSymbol :: CurrencySymbol
  , projectToken :: TokenName
  , raisingSymbol :: CurrencySymbol
  , raisingToken :: TokenName
  , presaleTierCs :: CurrencySymbol
  , tokensToDistribute :: Integer
  , withdrawalEndTime :: POSIXTime
  , oilAda :: Integer
  , commitFoldFeeAda :: Integer
  , -- if 0, only sundae pool is created
    -- if 10_000, only wr pool is created
    -- if 0 < splitBps < 10_000, splitBps determines what goes to Wr, the rest goes to Sundae
    -- NOTE: we don't ensure it's in the [0, 10_000] in the contracts, it's left to off-chain config creation
    splitBps :: Integer
  , daoFeeUnits :: Integer
  , daoFeeBase :: Integer
  , daoFeeReceiver :: Address
  , raisedTokensPoolPartPercentage :: Integer
  , collateral :: Integer
  }
  
data RewardsFoldDatum = RewardsFoldDatum
  { nodeScriptHash :: ScriptHash
  , next :: Maybe NodeKey
  , cutoffKey :: Maybe NodeKey
  , cutoffTime :: Maybe POSIXTime
  , committed :: Integer
  , overcommitted :: Integer
  , commitFoldOwner :: Address
  }

data RewardsFoldRedeemer
  = -- | Do a fold over the nodes and distribute the rewards.
    -- The commit fold compensation index,
    -- the dao compensation index,
    -- and the owner compensation index
    -- are ignored in all steps except the last.
    --          inputs    outputs   commit  fold    holder  dao     owner
    RewardsFold [Integer] [Integer] Integer Integer Integer Integer Integer
  | RewardsFoldEmergencyWithdrawal
```

The rewards fold supports three types of transactions, even though the same redeemer is used to indicate the first two:
- rewards fold not including the last node
- rewards fold including the final node
- emergency withdrawal

The middle rewards fold ensures a correct subset of nodes is spent, their commitments are either accumulated into a spent and recreated first project tokens holder, and the user rewards are distributed from that first project tokens holder into rewards holders.

The final rewards fold ensures a correct subset of nodes is spent including the final node, their commitments are collected, and the rewards are distributed from the first project tokens holder into rewards holders. The collected liquidity is then split between:
- the dao fee receiver which receives a preconfigured percentage of the collected tokens
- the launch owner who gets a preconfigured portion of the collected liquidity
- what's left is split between final tokens holders for each used DEX (at least one), this includes the remaining collected tokens and the remaining project tokens.
Additionally, the commit fold owner has to be compensated with Ada for his fold per each node folded over.

The emergency withdrawal flow exists in case something goes wrong. It allows spending the rewards fold utxo 30 days after the start of the launch by the commit fold owner.

### Rewards holder

The rewards holder is a validator-only contract.
It has the following configuration:
```haskell
data RewardsHolderConfig = RewardsHolderConfig
  { poolProofValidatorHash :: ScriptHash
  , poolProofSymbol :: CurrencySymbol
  , usesWr :: Bool
  , usesSundae :: Bool
  , withdrawalEndTime :: POSIXTime
  }

data RewardsHolderDatum = RewardsHolderDatum
  { owner :: NodeKey
  , projectSymbol :: CurrencySymbol
  , projectToken :: TokenName
  , raisingSymbol :: CurrencySymbol
  , raisingToken :: TokenName
  }
```

It ensures the users unlock their rewards in one of the two cases:
- a pool proof from a dex used in the launch is referenced and the owner signs the transaction
- or 30 days have passed since the start of the launch for cases where something has gone wrong and the launch can't finish.

It is important that users don't get priority access to project tokens based on ordering of their keys to preserve fairness. Using a pool proofs allows users to unlock their tokens simultaneously once the pool is created, and not during the rewards fold.

### Final project tokens holder

This contracts only has the validator with the following configuration:
```haskell
data TokensHolderFinalConfig = TokensHolderFinalConfig
  { owner :: Address
  , wrPoolSymbol :: CurrencySymbol
  , wrPoolValidatorHash :: ScriptHash
  , wrFactoryValidatorHash :: ScriptHash
  , sundaePoolScriptHash :: ScriptHash
  , sundaeFeeTolerance :: Integer
  , sundaeSettingsCurrencySymbol :: CurrencySymbol
  , poolProofValidatorHash :: ScriptHash
  , vestingValidatorHash :: ScriptHash
  , vestingPeriodDuration :: POSIXTime
  , vestingPeriodDurationToFirstUnlock :: POSIXTime
  , vestingPeriodInstallments :: Integer
  , vestingPeriodStart :: POSIXTime
  , daoFeeReceiver :: Address
  , raisingSymbol :: CurrencySymbol
  , raisingToken :: TokenName
  , projectSymbol :: CurrencySymbol
  , projectToken :: TokenName
  , starter :: TxOutRef
  }

-- Datum:
-- Encoded as an integer
data Dex = Wr | Sundae

-- Encoded as an integer
data TokensHolderFinalRedeemer
  = FailedFlow
  | NormalFlow
```

The validator supports two flows:
- the normal flow
- the failed flow

Additionally the stored datum indicates the DEX which influences the chosen validation.

The normal flow for both DEXes ensures a pool is created (by delegating most of the checks to a DEX-specific script) with the collected liquidity and the resulting shares are locked into a vesting utxo. Additionally, for Sundae the validator ensures the pool creation fee is acceptable for the launch owner. The fee is paid from the collected tokens if Ada is collected or it's supplied in a separate utxo in case a different token is collected.

The failed flow reason differs by DEX.
For WingRiders it can happen when a pool for the given asset pair already exists, since WingRiders supports only one pool pair for the protocol.
For Sundae it happens when the pool creation fee is above the tolerance configured during the creation of the launch.
In those cases the liquidity is transferred to the DAO fee receiver with the intention of creating a correct add liquidity request for WingRiders or negotiating with Sundae about lowering the pool creation fee.

### Pool proof

Has a validator and a minting policy in separate contracts.

The minting policy has the following configuration:
```haskell
data PoolProofPolicyConfig = PoolProofPolicyConfig
  { wrPoolValidatorHash :: ScriptHash
  , wrPoolSymbol :: CurrencySymbol
  , sundaePoolScriptHash :: ScriptHash
  -- ^ Both validator and minting policy share the script
  }
  
-- Redeemer
-- Encoded as an integer
data Dex = Wr | Sundae
```

The policy is responsible for ensuring the pool proof utxo is created correctly and that the transaction references a correct pool from a supported dex.

The validator has the following configuration:
```haskell
-- Parameter of the Pool Proof Validator (CurrencySymbol of the Pool Proof Token)
type PoolProofConfig = CurrencySymbol

-- Datum
-- Encoded as an integer
data Dex = Wr | Sundae

```

The pool proof validator always fails. Its purpose in to indicate an existence of a pool so it can be referenced by rewards holders (or final project tokens holder) so the users can unlock their rewards.

### Fail proof

Has a validator and a minting policy in separate contracts.

The minting policy doesn't have any parameters and ignores the redeemer.

The minting policy validates the correctness of the fail proof utxo output and delegates other validations to the node input with the correct redeemer.

The validator always errors out and has the following datum:
```haskell
-- Script hash of the node validator
type FailProofDatum = ScriptHash
```

The fail proof validator will never succeed in any transaction, it's supposed to indicate a failed launchpad so it can be referenced by node reclamation transactions.

## On-chain Linked List

To allow unbounded scaling with good concurrency, user commitments are represented as separate utxos which are conceptually linked together by their keys.

Let's take a look at how we represent a linked list on-chain:
```haskell
type NodeKey = (BuiltinByteString, Integer)

data Node = Node
  { key :: Maybe NodeKey
  , next :: Maybe NodeKey
  , createdTime :: POSIXTime
  , committed :: Integer
  }
```

Each node in the list is a separate utxo. They are identified by a key and are linked together with the next field holding the key of the element in the list. To inserting a new node, you find the correct parent, spend and recreate it with the updated next field in a transaction that creates the new element pointing to the next node of the spent one.

One of the invariants the contracts preserve is that ordering is deterministic, each key is sorted lexicographically before the next one. The keys are pairs of byte-arrays and integers.

There exist 3 types of nodes:
- Head node
- Separator nodes
- User nodes

The head node doesn't represent any commitment and exists to start the linked list. It has no key.

Separator nodes also don't contain any funds. They exist to facilitate better concurrency since without these chances are multiple users would have to insert their nodes onto the same parent node and only one transaction will pass. The byte-string part of the key for separator nodes are restricted to be one-byte long, this way they never overlap with user keys but still can be inserted to divide the whole key space uniformly into chunks.

User nodes use pub key hashes as byte-array parts. These nodes contain funds.

The integer part of the key allows users to insert multiple nodes without revoking their commitment first in case they want to commit more. This integer is restricted to be 0 ≤ i ≤ 255. Note that this gives maximum of 256 nodes per user, not per launch.
