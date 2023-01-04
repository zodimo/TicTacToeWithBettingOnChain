{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE FlexibleContexts    #-}  --Enable flexible contexts. Implied by ImplicitParams
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE ScopedTypeVariables #-}  --Enable lexical scoping of type variables explicit introduced with forall
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax
{-# LANGUAGE TypeApplications    #-}  --Allow the use of type application syntax
{-# LANGUAGE TypeFamilies        #-}  --Allow use and definition of indexed type and data families
{-# LANGUAGE TypeOperators       #-}  --Allow the use and definition of types with operator names
{-# LANGUAGE OverloadedStrings   #-}  --Allow string to be used for bytestring
{-# LANGUAGE RecordWildCards     #-}  --Allwow {..}

module GameValidator where

--PlutusTx 
import           PlutusTx                       (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins              as Builtins
import           PlutusTx.Prelude               hiding (Semigroup(..), unless)
--Contract Monad
import           Plutus.Contract               
--Ledger 
import           Ledger                         hiding (singleton)
import qualified Ledger.Address                 as Address
import           Ledger.Constraints             as Constraints              -- Same library name, different functions for V1 and V2 in some cases
--import qualified Ledger.Scripts               as Scripts               
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts            -- New library name for Typed Validators and some new fuctions
import qualified Plutus.V2.Ledger.Api                 as PlutusV2    
import           Ledger.Ada                     as Ada 
--Trace Emulator
import           Plutus.Trace
import qualified Plutus.Trace.Emulator          as Emulator
import qualified Wallet.Emulator.Wallet         as Wallet
--"Normal" Haskell -}
import           Control.Monad                  hiding (fmap)
import           Data.Map                       as Map
import           Data.Text                      (Text)
import           Data.Void                      (Void)
import           Prelude                        (IO, Semigroup (..), String, Show (..))
import           Text.Printf                    (printf)
import           Control.Monad.Freer.Extras     as Extras


-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

--THE ON-CHAIN CODE

-- custom data types for DATUMS

data Row = Row_A| Row_B | Row_C deriving Show
data Column = Col_1 | Col_2 | Col_3 deriving Show
data Move = Move Row Column deriving Show
data MoveMade = MoveMade PaymentPubKeyHash Move deriving Show
data Moves = Moves [MoveMade] deriving Show

-- template haskell to make instance of data for custom datatypes.
PlutusTx.makeIsDataIndexed ''Row [('Row_A,0),('Row_B,1),('Row_C,2)]
PlutusTx.makeIsDataIndexed ''Column [('Col_1,0),('Col_2,1),('Col_3,2)]
PlutusTx.makeIsDataIndexed ''Move [('Move,0)]
PlutusTx.makeIsDataIndexed ''MoveMade [('MoveMade,0)]
PlutusTx.makeIsDataIndexed ''Moves [('Moves,0)]


data GameStateDatum = GameInitiated
    { giPlayerOnePubKeyHash         :: BuiltinByteString
    , giBetInAda                    :: Integer
    , giGameMaxIntervalInSeconds    :: Integer
    , giOccurredAtPosixTime         :: Integer
    } | GameInProgress
    { gipPlayerOnePubKeyHash        :: BuiltinByteString
    , gipPlayerTwoPubKeyHash        :: BuiltinByteString
    , gipBetInAda                   :: Integer
    , gipGameMaxIntervalInSeconds   :: Integer
    , gipOccurredAtPosixTime        :: Integer
    , gipPlayerAddressToMakeMove    :: BuiltinByteString
    , gipMoves                      :: Moves
    } | GameIsWon
    { giwPlayerOnePubKeyHash        :: BuiltinByteString
    , giwPlayerTwoPubKeyHash        :: BuiltinByteString
    , giwBetInAda                   :: Integer
    , giwGameMaxIntervalInSeconds   :: Integer
    , giwOccurredAtPosixTime        :: Integer
    , giwWinningPlayerAddress       :: BuiltinByteString
    , giwMoves                      :: Moves
    } | GameIsTied
    { gitPlayerOnePubKeyHash        :: BuiltinByteString
    , gitPlayerTwoPubKeyHash        :: BuiltinByteString
    , gitBetInAda                   :: Integer
    , gitOccurredAtPosixTime        :: Integer
    , gitMoves                      :: Moves
    } deriving (Show)

-- template haskell to make instance of data for custom datatypes.
PlutusTx.makeIsDataIndexed ''GameStateDatum [('GameInitiated, 0), ('GameInProgress, 1), ('GameIsWon, 2), ('GameIsTied, 3)]

-- Custom DataTypes for Redeemers

data GameActionCommandRedeemer = JoinGameCommand
    { jgcPlayerTwoPubKeyHash     :: BuiltinByteString
    } | MakeMoveCommand
    { mmcPlayerPubKeyHash        :: BuiltinByteString
    , mmcMove                    :: Move
    } | ClaimWinCommand | ClaimTieCommand | CancelInitiatedGameCommand | CancelInProgressGameCommand

-- template haskell to make instance of data for custom datatypes.
PlutusTx.makeIsDataIndexed ''GameActionCommandRedeemer [ ('JoinGameCommand, 0)
                                             , ('MakeMoveCommand, 1)
                                             , ('ClaimWinCommand, 2)
                                             , ('ClaimTieCommand, 3)
                                             , ('CancelInitiatedGameCommand, 4)
                                             , ('CancelInProgressGameCommand, 5)
                                             ]



{-# INLINABLE mkValidator #-} -- Everything that its supposed to run in on-chain code need this pragma
 -- Datum -- Redeemer -- ScriptContext
-- mkValidator :: (GameStateDatum) -> () -> PlutusV2.ScriptContext -> Bool   -- the value of this function is on its sideeffects
mkValidator :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool   -- the value of this function is on its sideeffects
-- | gamestate == invalid and pkh == rootPkh = True
-- gamestate is derived from datum on utxo and provided datums and redeemer.
mkValidator gameState actionCommand ctx =  traceIfFalse "Invalid Command for GameState" $ validActionForState gameState actionCommand ctx

-- helper functions.

-- only certain combinations are allowed.
{-# INLINABLE validActionForState #-}
validActionForState :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
validActionForState gs command ctx = 
    case gs of
        GameInitiated {}  -> validGameInitiatedCommand gs command ctx
        GameInProgress {} -> validGameInProgress gs command ctx
        GameIsWon {}      -> validGameIsWonCommand gs command ctx
        GameIsTied {}     -> validGameIsTiedCommand gs command ctx
        
{-# INLINABLE validGameInitiatedCommand #-}
validGameInitiatedCommand :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
validGameInitiatedCommand gs command ctx = case command of
                                JoinGameCommand {}          -> canJoinGame gs command ctx
                                CancelInitiatedGameCommand  -> canCancelInitiatedGame gs command ctx
                                _                           -> False
                                
{-# INLINABLE validGameInProgress #-}
validGameInProgress :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
validGameInProgress gs command ctx = case command of
                                MakeMoveCommand {}          -> canMakeMove gs command ctx
                                CancelInProgressGameCommand -> canCancelInProgressGame gs command ctx
                                _                           -> False

{-# INLINABLE validGameIsWonCommand #-}
validGameIsWonCommand :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
validGameIsWonCommand gs command ctx = case command of
                                ClaimWinCommand             -> canClaimWin gs command ctx
                                _                           -> False

{-# INLINABLE validGameIsTiedCommand #-}
validGameIsTiedCommand :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
validGameIsTiedCommand gs command ctx = case command of
                                ClaimTieCommand             -> canClaimTie gs command ctx
                                _                           -> False

{- LETS GET TO BUSINESS -}

-- match bet in value
-- need access to txInfo
-- output to script must batch the bet
{-# INLINABLE canJoinGame #-}
canJoinGame :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
canJoinGame _ _ _ = True

-- txInfoValidRange :: POSIXTimeRange > ((giOccurredAtPosixTime GameStateDatum) + (giGameMaxIntervalInSeconds *1000))
-- validate output goes back to original wallet
{-# INLINABLE canCancelInitiatedGame #-}
canCancelInitiatedGame :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
canCancelInitiatedGame _ _ _ = True

-- move not made before
-- validate output
-- output is still in progress, or won , or tied
{-# INLINABLE canMakeMove #-}
canMakeMove :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
canMakeMove _ _ _ = True

-- txInfoValidRange :: POSIXTimeRange > ((giOccurredAtPosixTime GameStateDatum) + (giGameMaxIntervalInSeconds *1000))
-- validate output , the winner is the player who is waiting for the other player to play
{-# INLINABLE canCancelInProgressGame #-}
canCancelInProgressGame :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
canCancelInProgressGame _ _ _ = True

-- validate output
-- value goes to winner
{-# INLINABLE canClaimWin #-}
canClaimWin :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
canClaimWin _ _ _ = True

-- validate output
-- value is split evenly
{-# INLINABLE canClaimTie #-}
canClaimTie :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
canClaimTie _ _ _ = True


{- 
This section is where the logic of the game will be duplicated from the dapp.
duplicated ? frontend and backend validation.

each transaction has 3 things, input/inline datum

    Datum and redeemer combination = transition

    GameInitiated + joinGame  = GameInProgress
    GameInProgress + MakeMoveCommand = GameInProgress || GameIsWon || GameIsTied
    GameIsWon  + ClaimWinCommand  = The end
    GameIsTied + ClaimTieCommand  = The end
    GameInitiated + CancelInitiatedGameCommand = the end
    GameInProgress + CancelInProgressGameCommand = the end

-}



data Game
instance Scripts.ValidatorTypes Game where
    -- type instance DatumType Game = GameStateDatum
    type instance DatumType Game = GameStateDatum
    type instance RedeemerType Game = GameActionCommandRedeemer


typedValidator :: Scripts.TypedValidator Game
typedValidator = Scripts.mkTypedValidator @Game
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    -- wrap = Scripts.mkUntypedValidator @GameStateDatum @()
    wrap = Scripts.mkUntypedValidator @GameStateDatum @GameActionCommandRedeemer

    

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = Scripts.validatorAddress typedValidator

