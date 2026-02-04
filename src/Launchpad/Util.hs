{-# LANGUAGE BlockArguments #-}

module Launchpad.Util where

import Launchpad.Types
import Plutarch.Api.V1.Value (padaSymbol, padaToken, pvalueOf)
import Plutarch.Api.V2
import Plutarch.Mint.Util
import Plutarch.Prelude
import Plutarch.Util

ptierParams ::
  Term s PTier ->
  (Term s PInteger, Term s PInteger, Term s PPOSIXTime) ->
  (Term s PInteger, Term s PInteger, Term s PPOSIXTime) ->
  Term s (PPair (PPair PInteger PInteger) PPOSIXTime)
ptierParams
  tier
  (defaultTierMinCommitment, defaultTierMaxCommitment, defaultStartTime)
  (presaleTierMinCommitment, presaleTierMaxCommitment, presaleTierStartTime) =
    pmatch tier \case
      PPresale ->
        ppair (ppair presaleTierMinCommitment presaleTierMaxCommitment) presaleTierStartTime
      PDefault ->
        ppair (ppair defaultTierMinCommitment defaultTierMaxCommitment) defaultStartTime

ptierCs :: Term s PCurrencySymbol -> Term s PTier -> Term s (PMaybe PCurrencySymbol)
ptierCs presaleCs tier =
  pmatch tier \case
    PPresale -> pjust presaleCs
    PDefault -> pnothing

{- | Check if the value of the node is correct.
The correct node value includes:
  - one node token
  - the correct expected amount of oil ADA
  - the correct expected amount of committed tokens
  - one associated tier-specific token when the tier is used
-}
pisNodeValueCorrect ::
  Term
    s
    ( PCurrencySymbol
        :--> PTokenName
        :--> PCurrencySymbol
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PTier
        :--> PValue 'Sorted 'Positive
        :--> PInteger
        :--> PInteger
        :--> PBool
    )
pisNodeValueCorrect = phoistAcyclic $ plam \raisingSymbol raisingToken presaleCs nodeCs nodeTn tier value committed expectedOil ->
  pand'List
    [ ptraceIfFalse "nvc0" $ pvalueOf # value # nodeCs # nodeTn #== 1
    , pmatch (ptierCs presaleCs tier) \case
        PJust tierCs -> ptraceIfFalse "nvc1" $ psnd # (pvalueOfSingleton' # tierCs # value) #== 1
        PNothing -> ptrue
    , pif
        (pisAda # raisingSymbol)
        ( pand'List
            [ ptraceIfFalse "nvc2" $ pvalueOf # value # padaSymbol # padaToken #>= expectedOil + committed
            , ptraceIfFalse "nvc3" $ pcountOfUniqueTokens # value #<= 3
            ]
        )
        ( pand'List
            [ ptraceIfFalse "nvc4" $ pvalueOf # value # padaSymbol # padaToken #>= expectedOil
            , ptraceIfFalse "nvc5" $ pvalueOf # value # raisingSymbol # raisingToken #== committed
            , ptraceIfFalse "nvc6" $ pcountOfUniqueTokens # value #<= 4
            ]
        )
    ]

pisCorrectPool ::
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  Term s PBool
pisCorrectPool (projectSymbol, projectToken) (raisingSymbol, raisingToken) (assetASymbol, assetAToken) (assetBSymbol, assetBToken) =
  pcond
    [ (projectSymbol #< raisingSymbol, projectIsA'n'raisingIsB)
    , (projectSymbol #> raisingSymbol, raisingIsA'n'projectIsB)
    , (projectToken #< raisingToken, projectIsA'n'raisingIsB)
    ]
    raisingIsA'n'projectIsB
  where
    projectIsA'n'raisingIsB =
      pand'List
        [ projectSymbol #== assetASymbol
        , projectToken #== assetAToken
        , raisingSymbol #== assetBSymbol
        , raisingToken #== assetBToken
        ]
    raisingIsA'n'projectIsB =
      pand'List
        [ raisingSymbol #== assetASymbol
        , raisingToken #== assetAToken
        , projectSymbol #== assetBSymbol
        , projectToken #== assetBToken
        ]

-- https://github.com/SundaeSwap-finance/sundae-contracts/blob/be33466b7dbe0f8e6c0e0f46ff23737897f45835/lib/shared.ak#L222
poolSundaeNftName :: Term s PByteString -> Term s PTokenName
poolSundaeNftName identifier = pcon . PTokenName $ (pconstant @PByteString "000de140") <> identifier

-- https://github.com/SundaeSwap-finance/sundae-contracts/blob/be33466b7dbe0f8e6c0e0f46ff23737897f45835/lib/shared.ak#L228
poolSundaeLpName :: Term s PByteString -> Term s PTokenName
poolSundaeLpName identifier = pcon . PTokenName $ (pconstant @PByteString "0014df10") <> identifier
