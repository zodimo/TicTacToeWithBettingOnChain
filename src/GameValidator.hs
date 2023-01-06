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
-- import qualified Ledger.Address                 as Address
-- import           Ledger.Constraints             as Constraints              -- Same library name, different functions for V1 and V2 in some cases
--import qualified Ledger.Scripts               as Scripts               
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts            -- New library name for Typed Validators and some new fuctions
import qualified Plutus.V2.Ledger.Api                 as PlutusV2    
import qualified Plutus.V2.Ledger.Contexts            as PlutusV2    

import           Plutus.V1.Ledger.Interval                  (contains)
import qualified Ledger.Ada                 as Ada

--"Normal" Haskell -}
import           Control.Monad                  hiding (fmap)
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
data MoveMade = MoveMade 
    { mmPlayerPubKeyHash::BuiltinByteString
    , mmMove::Move
    } deriving Show
data MovesMade = MovesMade [MoveMade] deriving Show
data Moves = Moves [Move] deriving Show

-- template haskell to make instance of data for custom datatypes.
PlutusTx.makeIsDataIndexed ''Row [('Row_A,0),('Row_B,1),('Row_C,2)]
PlutusTx.makeIsDataIndexed ''Column [('Col_1,0),('Col_2,1),('Col_3,2)]
PlutusTx.makeIsDataIndexed ''Move [('Move,0)]
-- crossing of concersn with names of moves and movesMade and move
PlutusTx.makeIsDataIndexed ''MoveMade [('MoveMade,0)]
PlutusTx.makeIsDataIndexed ''MovesMade [('MovesMade,0)]
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
    , gipMovesMade                  :: MovesMade
    } | GameIsWon
    { giwPlayerOnePubKeyHash        :: BuiltinByteString
    , giwPlayerTwoPubKeyHash        :: BuiltinByteString
    , giwBetInAda                   :: Integer
    , giwGameMaxIntervalInSeconds   :: Integer
    , giwOccurredAtPosixTime        :: Integer
    , giwWinningPlayerAddress       :: BuiltinByteString
    , giwMovesMade                  :: MovesMade
    } | GameIsTied
    { gitPlayerOnePubKeyHash        :: BuiltinByteString
    , gitPlayerTwoPubKeyHash        :: BuiltinByteString
    , gitBetInAda                   :: Integer
    , gitOccurredAtPosixTime        :: Integer
    , gitMovesMade                  :: MovesMade
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

-- state transitions

-- {-# INLINABLE joinGame #-}
-- joinGame :: GameStateDatum -> GameActionCommandRedeemer -> GameStateDatum
-- joinGame gs command 
--         | GameInitiated{..} JoinGameCommand{..} = GameInProgress
--                                                     { gipPlayerOnePubKeyHash        = giPlayerOnePubKeyHash GameInitiated
--                                                     , gipPlayerTwoPubKeyHash        = jgcPlayerTwoPubKeyHash JoinGameCommand
--                                                     , gipBetInAda                   = giBetInAda GameInitiated
--                                                     , gipGameMaxIntervalInSeconds   = giGameMaxIntervalInSeconds GameInitiated
--                                                     , gipOccurredAtPosixTime        = giOccurredAtPosixTime  GameInitiated 
--                                                     -- gipPlayerAddressToMakeMove should have been on the join command
--                                                     -- but now we choose player 2 to always start.
--                                                     , gipPlayerAddressToMakeMove    = jgcPlayerTwoPubKeyHash JoinGameCommand
--                                                     , gipMoves                      = Moves []
--                                                     }
--         | otherwise                             = traceError "Invalid game state, cannot join game!"


-- {-# INLINABLE joinGame #-}
-- joinGame :: GameStateDatum -> GameActionCommandRedeemer -> GameStateDatum
-- joinGame gs command 
--         | GameInitiated{..} JoinGameCommand{..} = GameInProgress
--                                                     { gipPlayerOnePubKeyHash        = giPlayerOnePubKeyHash GameInitiated
--                                                     , gipPlayerTwoPubKeyHash        = jgcPlayerTwoPubKeyHash JoinGameCommand
--                                                     , gipBetInAda                   = giBetInAda GameInitiated
--                                                     , gipGameMaxIntervalInSeconds   = giGameMaxIntervalInSeconds GameInitiated
--                                                     , gipOccurredAtPosixTime        = giOccurredAtPosixTime  GameInitiated 
--                                                     -- gipPlayerAddressToMakeMove should have been on the join command
--                                                     -- but now we choose player 2 to always start.
--                                                     , gipPlayerAddressToMakeMove    = jgcPlayerTwoPubKeyHash JoinGameCommand
--                                                     , gipMoves                      = Moves []
--                                                     }
--         | otherwise                             = traceError "Invalid game state, cannot join game!"


instance Eq Row where
    {-# INLINABLE (==) #-}
    Row_A == Row_A = True
    Row_B == Row_B = True
    Row_C == Row_C = True
    Row_A == Row_B = False
    Row_B == Row_A = False
    Row_A == Row_C = False
    Row_C == Row_A = False
    Row_B == Row_C = False
    Row_C == Row_B = False


instance Eq Column where
    {-# INLINABLE (==) #-}
    Col_1 == Col_1 = True
    Col_2 == Col_2 = True
    Col_3 == Col_3 = True
    Col_1 == Col_2 = False
    Col_2 == Col_1 = False
    Col_1 == Col_3 = False
    Col_3 == Col_1 = False
    Col_2 == Col_3 = False
    Col_3 == Col_2 = False

instance Eq Move where
    {-# INLINABLE (==) #-}
    (Move aRow aCol) == (Move bRow bCol) = (aRow == bRow) && (aCol == bCol)


{-# INLINABLE moveMadeToMove #-}
moveMadeToMove :: MoveMade -> Move
moveMadeToMove (MoveMade _ move) = move

{-# INLINABLE movesMadeToMoves #-}
movesMadeToMoves :: [MoveMade] -> [Move]
movesMadeToMoves = map moveMadeToMove


-- not been played
{-# INLINABLE isMoveAvailable #-}
isMoveAvailable :: Move -> [Move] -> Bool
isMoveAvailable move  moves = move `notElem` moves

{-# INLINABLE appendToMovesMade #-}
appendToMovesMade :: MoveMade -> [MoveMade] -> [MoveMade] 
appendToMovesMade  a [] =  [a]
appendToMovesMade  a (x:xs) = x : appendToMovesMade a xs

{-# INLINABLE unwrapMovesMade #-}
unwrapMovesMade :: MovesMade -> [MoveMade]
unwrapMovesMade (MovesMade a) = a

{-# INLINABLE unwrapMoves #-}
unwrapMoves :: Moves -> [Move]
unwrapMoves (Moves a) = a

{-# INLINABLE isMoveMadeInTopRow #-}
isMoveMadeInTopRow:: MoveMade ->Bool
isMoveMadeInTopRow (MoveMade _ (Move row _)) = case row of 
        Row_A  -> True
        _      -> False

{-# INLINABLE topRowFromMovesMade #-}
topRowFromMovesMade :: [MoveMade]->[MoveMade]
topRowFromMovesMade = filter isMoveMadeInTopRow
    
{-# INLINABLE isTopRowFilled #-}
isTopRowFilled:: MovesMade -> Bool
isTopRowFilled (MovesMade movesMade) =  length (topRowFromMovesMade movesMade ) == 3

{-# INLINABLE isRowFilledByPlayer #-}
isRowFilledByPlayer :: BuiltinByteString -> [MoveMade] -> Bool
isRowFilledByPlayer playerPubKeyHash movesMade = 
    (all (== playerPubKeyHash) (map (\moveMade -> mmPlayerPubKeyHash moveMade) movesMade))


{-# INLINABLE isTopRowFilledByPlayer #-}
isTopRowFilledByPlayer :: BuiltinByteString -> MovesMade -> Bool
isTopRowFilledByPlayer playerPubKeyHash (MovesMade movesMade) = (isTopRowFilled (MovesMade movesMade)) &&
        let topRow = topRowFromMovesMade movesMade
        in isRowFilledByPlayer playerPubKeyHash topRow

{-# INLINABLE isGameTied #-}
isGameTied :: BuiltinByteString -> BuiltinByteString -> [MoveMade] -> Bool
-- top row filled but nobody won
isGameTied  p1pkh p2pkh movesMade =  all ( == False) [ (isGameWonByPlayer p1pkh (MovesMade movesMade)), (isGameWonByPlayer p2pkh (MovesMade movesMade)) ]

{-# INLINABLE isGameWon #-}
isGameWon ::  BuiltinByteString -> BuiltinByteString -> [MoveMade] -> Bool
isGameWon  p1pkh p2pkh movesMade =  elem True [ (isGameWonByPlayer p1pkh (MovesMade movesMade)), (isGameWonByPlayer p2pkh (MovesMade movesMade)) ]


{-# INLINABLE isGameWonByPlayer #-}
isGameWonByPlayer :: BuiltinByteString -> MovesMade -> Bool
-- top row filled by same player
isGameWonByPlayer = isTopRowFilledByPlayer

{-# INLINABLE getWinningPlayerPubKeyHash #-}
getWinningPlayerPubKeyHash :: BuiltinByteString -> BuiltinByteString -> MovesMade -> BuiltinByteString
getWinningPlayerPubKeyHash p1pkh p2pkh (MovesMade movesMade)  = 
    case (isGameWon p1pkh p2pkh movesMade) of
        False -> traceError "Nobody Won!"
        True  -> case (isGameWonByPlayer p1pkh (MovesMade movesMade)) of
            True -> p1pkh
            _    -> p2pkh


{-# INLINABLE isMoveAvailableInThisGame #-}
isMoveAvailableInThisGame :: GameStateDatum -> GameActionCommandRedeemer ->Bool
isMoveAvailableInThisGame gip command  = case gip of
    GameInProgress{..} -> case command of
            MakeMoveCommand{..} -> traceIfFalse "Illegal move: position is occupied." (isMoveAvailable mmcMove ( movesMadeToMoves (unwrapMovesMade gipMovesMade)))
            _                   -> traceError "expected MakeMoveCommand"
    _                  -> traceError "expected GameInProgress"


{-# INLINABLE playerInCommandMustMatchNextPlayerInGameState #-}
playerInCommandMustMatchNextPlayerInGameState :: GameStateDatum -> GameActionCommandRedeemer -> Bool
playerInCommandMustMatchNextPlayerInGameState gs command = case gs of 
        GameInProgress {..}           -> case command of
            MakeMoveCommand {..}          ->  traceIfFalse "Wrong player playing now!"
                $ gipPlayerAddressToMakeMove == mmcPlayerPubKeyHash 
            _                   -> traceError "expected MakeMoveCommand"
        _                   -> traceError "expected GameInProgress"

-- getWinner :: Moves -> BuiltinByteString


-- {-# INLINABLE getWinnerPubKeyHashFromMovesMade #-}
-- -- The Winning move was the last move if the game is in a winning state
-- getWinnerPubKeyHashFromMovesMade :: MovesMade -> BuiltinByteString
-- -- getWinnerPubKeyHashFromMovesMade movesMade = mmPlayerPubKeyHash getLastMoveMadeFromMovesMade movesMade
-- getWinnerPubKeyHashFromMovesMade = mmPlayerPubKeyHash getLastMoveMadeFromMovesMade

-- {-# INLINABLE getLastMoveMadeFromMovesMade #-}
-- getLastMoveMadeFromMovesMade :: MovesMade -> MoveMade
-- getLastMoveMadeFromMovesMade MovesMade movesMade
--     | null movesMade    =  traceError "No Moves have been made"
--     | otherwise         =last xs

-- {-# INLINABLE mkMoveMadeFromMakeMoveCommand #-}
-- mkMoveMadeFromMakeMoveCommand :: GameActionCommandRedeemer -> MoveMade
-- mkMoveMadeFromMakeMoveCommand  MakeMoveCommand{..} = MoveMade mmcPlayerPubKeyHash mmcMove
 



{-# INLINABLE makeMove #-}
-- gamestate already validated to be GameInProgress
makeMove :: GameStateDatum -> GameActionCommandRedeemer -> GameStateDatum
makeMove gip command =   case gip of
    GameInProgress{..} -> case command of
        MakeMoveCommand{..} -> let movesMade =  appendToMovesMade (MoveMade mmcPlayerPubKeyHash mmcMove) (unwrapMovesMade gipMovesMade)
                        in if isGameWon gipPlayerOnePubKeyHash gipPlayerTwoPubKeyHash movesMade then
                            GameIsWon 
                            { giwPlayerOnePubKeyHash         = gipPlayerOnePubKeyHash 
                            , giwPlayerTwoPubKeyHash        = gipPlayerTwoPubKeyHash 
                            , giwBetInAda                   = gipBetInAda 
                            , giwGameMaxIntervalInSeconds   = gipGameMaxIntervalInSeconds 
                            , giwOccurredAtPosixTime        = gipOccurredAtPosixTime 
                            , giwWinningPlayerAddress       = getWinningPlayerPubKeyHash  gipPlayerOnePubKeyHash gipPlayerTwoPubKeyHash (MovesMade movesMade)
                            , giwMovesMade                  = MovesMade movesMade

                            }
                            else ( 
                                if isGameTied gipPlayerOnePubKeyHash gipPlayerTwoPubKeyHash movesMade then
                                    GameIsTied
                                    { gitPlayerOnePubKeyHash        = gipPlayerOnePubKeyHash 
                                    , gitPlayerTwoPubKeyHash        = gipPlayerTwoPubKeyHash 
                                    , gitBetInAda                   = gipBetInAda 
                                    , gitOccurredAtPosixTime        = gipOccurredAtPosixTime 
                                    , gitMovesMade                  = MovesMade movesMade
                                    }
                                else 
                                    GameInProgress {gipMovesMade = MovesMade movesMade, ..}
                                )
        _                   -> traceError "expected MakeMoveCommand"
    _                   -> traceError "expected GameInProgress"

            





{-# INLINABLE mkValidator #-} -- Everything that its supposed to run in on-chain code need this pragma
 -- Datum -- Redeemer -- ScriptContext
-- mkValidator :: (GameStateDatum) -> () -> PlutusV2.ScriptContext -> Bool   -- the value of this function is on its sideeffects
mkValidator :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool   -- the value of this function is on its sideeffects
-- | gamestate == invalid and pkh == rootPkh = True
-- gamestate is derived from datum on utxo and provided datums and redeemer.
mkValidator gameState actionCommand ctx =  traceIfFalse "Invalid Command for GameState" $ validCommandForGameState gameState actionCommand ctx
    where 
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

-- helper functions.

-- only certain combinations are allowed.
{-# INLINABLE validCommandForGameState #-}
validCommandForGameState :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
validCommandForGameState gs command ctx =  case gs of
                                GameInitiated {}            -> validGameInitiatedCommand gs command ctx
                                GameInProgress {}           -> validGameInProgress gs command ctx
                                GameIsWon {}                -> validGameIsWonCommand gs command ctx
                                GameIsTied {}               -> validGameIsTiedCommand gs command ctx
        
    where 
        validGameInitiatedCommand :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
        validGameInitiatedCommand gs command ctx = case command of
                                        JoinGameCommand {}          -> canJoinGame gs command ctx
                                        CancelInitiatedGameCommand  -> canCancelInitiatedGame gs command ctx
                                        _                           -> traceError "Invalid command for gamestate GameInitiated"
                                

        validGameInProgress :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
        -- validGameInProgress gs command ctx = case command of
        validGameInProgress gs command ctx = case command of
                                        MakeMoveCommand {}          -> canMakeMove gs command ctx
                                        CancelInProgressGameCommand -> canCancelInProgressGame gs command ctx
                                        _                           -> traceError "Invalid command for gamestate GameInProgress"


        validGameIsWonCommand :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
        validGameIsWonCommand gs command ctx = case command of
                                        ClaimWinCommand             -> canClaimWin gs command ctx
                                        _                           -> traceError "Invalid command for gamestate GameIsWon"


        validGameIsTiedCommand :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
        validGameIsTiedCommand gs command ctx = case command of
                                        ClaimTieCommand             -> canClaimTie gs command ctx
                                        _                           -> traceError "Invalid command for gamestate GameIsTied"

{- LETS GET TO BUSINESS -}

-- match bet in value
-- can only join game value in utx matches bet
-- need access to txInfo
-- output to script must batch the bet
-- confirm output state is correct
-- GAME constraint input game state + command params = output game state
-- TX Contraint Constraints.mustPayToTheScript 




{-# INLINABLE canJoinGame #-}
canJoinGame :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
-- canJoinGame _ _ _ = True
canJoinGame gs _ ctx =  case gs of 
    GameInitiated {..} -> gameBetMustMatchTheValue && outputValueOnScriptMustBeDoubleTheBet
        where 
            -- the player initiating the game needs to match the value with the bet
            gameBetMustMatchTheValue :: Bool
            gameBetMustMatchTheValue =  traceIfFalse "Invalid initiated game. Value does not match bet" 
                                        $ (Ada.lovelaceValueOf (giBetInAda*1000000)) == getInputScriptValue ctx

            outputValueOnScriptMustBeDoubleTheBet :: Bool
            -- naive approach
            -- getContinuingOutputs and ensure the value has doubled
            -- take script value or value in bet ?
            outputValueOnScriptMustBeDoubleTheBet = let expectedAdaValue = Ada.fromValue $ Ada.lovelaceValueOf (2 * giBetInAda * 1000000)
                                                        in traceIfFalse "Bet is not matched, player cannot join!"  
                                                        $ expectedAdaValue == Ada.fromValue ( getOutputScriptValue ctx)
                                        
    _                  -> traceError "expected GameInitiated" 


{-# INLINABLE getOutputScriptValue #-}
getOutputScriptValue :: PlutusV2.ScriptContext -> PlutusV2.Value
-- output ensure there is only 1 and return value
-- what about if multiple games are played at once in a single transaction ? not supported at this time
getOutputScriptValue ctx = PlutusV2.txOutValue scriptTxOut
        where
            scriptTxouts = PlutusV2.getContinuingOutputs ctx
            scriptTxOut = case scriptTxouts of
                            [i] -> i
                            _   -> traceError "expected exactly one script output"   



{-# INLINABLE getInputScriptValue #-}
getInputScriptValue :: PlutusV2.ScriptContext -> PlutusV2.Value
getInputScriptValue ctx = PlutusV2.txOutValue . PlutusV2.txInInfoResolved $ scriptInputTxInInfo
        where            
            {- maybe refactor to use findOwnInput :: ScriptContext -> Maybe TxInInfo -}
            info :: PlutusV2.TxInfo
            info = PlutusV2.scriptContextTxInfo ctx

            hasTxInInfoDatum :: PlutusV2.TxInInfo -> Bool
            hasTxInInfoDatum txInInfo =  case (PlutusV2.txOutDatum . PlutusV2.txInInfoResolved $ txInInfo) of
                                                PlutusV2.NoOutputDatum        -> False
                                                _    -> True
                                                
            getTxInInfoWithDatum :: [PlutusV2.TxInInfo] -> PlutusV2.TxInInfo
            getTxInInfoWithDatum txInInfos = let 
                                                xs = filter hasTxInInfoDatum txInInfos
                                            in
                                                case xs of
                                                    [i] -> i
                                                    _   -> traceError "expected exactly one input with datum"   

            -- filter function
            getSriptInputTxInInfoFromTxInfo :: PlutusV2.TxInfo -> PlutusV2.TxInInfo
            getSriptInputTxInInfoFromTxInfo txInfo = getTxInInfoWithDatum (PlutusV2.txInfoInputs txInfo)
            
            -- get the scriptInputTxInInfo
            scriptInputTxInInfo :: PlutusV2.TxInInfo
            scriptInputTxInInfo = getSriptInputTxInInfoFromTxInfo info


{-
    correctValueInScriptToMatchBet 
    the joining player must match the bet with value
-}

{-
    correctOutputGameState
    -- players pkh correctly set
    -- bet correctly set
    -- moves are empty
     ...
-}



-- txInfoValidRange :: POSIXTimeRange > ((giOccurredAtPosixTime GameStateDatum) + (giGameMaxIntervalInSeconds *1000))
-- validate output goes back to original wallet - aka playerOnePubKeyHash
{-# INLINABLE canCancelInitiatedGame #-}
canCancelInitiatedGame :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
canCancelInitiatedGame gs command ctx = enoughTimeHasPassed gs txTimeRange && playerOneBetIsRefunded gs ctx 
    where 
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

        txTimeRange :: PlutusV2.POSIXTimeRange
        txTimeRange =  PlutusV2.txInfoValidRange info

{-# INLINABLE playerOneBetIsRefunded #-}
playerOneBetIsRefunded :: GameStateDatum->PlutusV2.ScriptContext->Bool
playerOneBetIsRefunded _ _ = True

-- move not made before
-- validate output
-- is correct player to make move
-- output is still in progress, or won , or tied
{-# INLINABLE canMakeMove #-}
canMakeMove :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
canMakeMove gs command _ = 
    (isMoveAvailableInThisGame gs command) &&
    (playerInCommandMustMatchNextPlayerInGameState gs command) && 
    True -- && True is placholder for more checks


-- txInfoValidRange :: POSIXTimeRange > ((giOccurredAtPosixTime GameStateDatum) + (giGameMaxIntervalInSeconds *1000))
-- validate output , the winner is the player who is waiting for the other player to play
-- mustPayToPubKey
{-# INLINABLE canCancelInProgressGame #-}
canCancelInProgressGame :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
canCancelInProgressGame gs command ctx = enoughTimeHasPassed gs txTimeRange  && winnerByTimeoutIsPaid gs ctx
    where 
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

        txTimeRange :: PlutusV2.POSIXTimeRange
        txTimeRange =  PlutusV2.txInfoValidRange info



-- {-# INLINABLE switchPLayer #-}
-- switchPLayer :: GameStateDatum BuiltinByteString -> BuiltinByteString

{-# INLINABLE getWinnerPubKeyHashByTimeout #-}
getWinnerPubKeyHashByTimeout :: GameStateDatum -> PubKeyHash
getWinnerPubKeyHashByTimeout gs = case gs of 
            GameInitiated{..}   ->  PubKeyHash giPlayerOnePubKeyHash
            GameInProgress{..}  ->  if gipPlayerOnePubKeyHash == gipPlayerAddressToMakeMove 
                                    then PubKeyHash gipPlayerTwoPubKeyHash 
                                    else PubKeyHash gipPlayerOnePubKeyHash
            _                   -> traceError "Expected game state if either GameInitiated or GameInProgress !"



{-# INLINABLE winnerByTimeoutIsPaid #-}
winnerByTimeoutIsPaid :: GameStateDatum -> PlutusV2.ScriptContext -> Bool
winnerByTimeoutIsPaid gs ctx = let  winnerPubKeyHash = getWinnerPubKeyHashByTimeout gs
                                    winningValueInAda = getInputScriptValue ctx
                                    valuePayToWinner = PlutusV2.valuePaidTo (PlutusV2.scriptContextTxInfo ctx) winnerPubKeyHash
                                    in 
                                        -- Very NAIVE!!
                                        -- the value paid to winner may be more that the bet as it may include the change  
                                        -- bypassed for now with True ||
                                        traceIfFalse "Winner is not getting paid!" 
                                        $ Ada.fromValue winningValueInAda <= Ada.fromValue valuePayToWinner


                


{-# INLINABLE enoughTimeHasPassed #-}
enoughTimeHasPassed :: GameStateDatum -> PlutusV2.POSIXTimeRange-> Bool
-- Disable this check for now with True ||
enoughTimeHasPassed gs txTimeRange = True || case gs of
        GameInitiated {..}  -> traceIfFalse "Not enough time has passed to cancel an initiated game."  
            ( contains (PlutusV2.from ( PlutusV2.POSIXTime $ giOccurredAtPosixTime + (giGameMaxIntervalInSeconds * 1000))) txTimeRange)
        GameInProgress {..}  -> traceIfFalse "Not enough time has passed to cancel an inprogress game."  
            ( contains (PlutusV2.from ( PlutusV2.POSIXTime $ gipOccurredAtPosixTime + (gipGameMaxIntervalInSeconds * 1000))) txTimeRange)
        _                   -> traceError "expected GameInitiated or GameInProgress"


-- validate output
-- value goes to winner
-- mustPayToPubKey
{-# INLINABLE canClaimWin #-}
canClaimWin :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
canClaimWin gs command ctx = case gs of 
    GameIsWon {..}          ->  let 
                                    winnerPubKeyHash = PubKeyHash giwWinningPlayerAddress   
                                    -- value in script of double the bet ?                             
                                    -- winningValue = (Ada.lovelaceValueOf (giBetInAda*1000000*2))
                                    winningValueInAda = getInputScriptValue ctx
                                    valuePayToWinner = PlutusV2.valuePaidTo (PlutusV2.scriptContextTxInfo ctx) winnerPubKeyHash
                                    in 
                                        -- Very NAIVE!!
                                        -- the value paid to winner may be more that the bet as it may include the change  
                                        -- bypassed for now with True ||
                                        traceIfFalse "Winner is not getting paid!" 
                                        $ Ada.fromValue winningValueInAda <= Ada.fromValue valuePayToWinner
                                                          
    _                       -> traceError "expected game state GameIsWon" 

        

-- validate output
-- value is split evenly
-- mustPayToPubKey
{-# INLINABLE canClaimTie #-}
canClaimTie :: GameStateDatum -> GameActionCommandRedeemer -> PlutusV2.ScriptContext -> Bool
canClaimTie gs command ctx = case gs of 
    GameIsTied {..} ->  let 

                        player1PubKeyHash = PubKeyHash gitPlayerOnePubKeyHash   
                        player2PubKeyHash = PubKeyHash gitPlayerTwoPubKeyHash         

                        -- refund of bets
                        payoutValueInAda = Ada.fromValue $ Ada.lovelaceValueOf (gitBetInAda*1000000)
                        valueInAdaPaidToPlayer1 = Ada.fromValue $ PlutusV2.valuePaidTo (PlutusV2.scriptContextTxInfo ctx) player1PubKeyHash
                        valueInAdaPaidToPlayer2 = Ada.fromValue $ PlutusV2.valuePaidTo (PlutusV2.scriptContextTxInfo ctx) player2PubKeyHash
                                            
                        in 
                            -- Very NAIVE!!
                            -- the value paid to each player may be more that the bet as it may include the change 
                            -- pay atleast the value of the initial bet  
                            traceIfFalse "Bets are not being refunded!"  
                            $ traceIfFalse "valuePayToPlayer1, bet is not being refunded!" (payoutValueInAda <= valueInAdaPaidToPlayer1) &&
                            traceIfFalse "valuePayToPlayer2, bet is not being refunded!" (payoutValueInAda <= valueInAdaPaidToPlayer2) 
                                                          
    _               -> traceError "expected game state GameIsTied" 

        

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

    

validator :: PlutusV2.Validator
validator = Scripts.validatorScript typedValidator

valHash :: PlutusV2.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: PlutusV2.Address
scrAddress = Scripts.validatorAddress typedValidator



{-

BELOW are test used in repl

-}
moveA1 = Move Row_A Col_1
moveA2 = Move Row_A Col_2
moveA3 = Move Row_A Col_3
moveB1 = Move Row_B Col_1
moveB2 = Move Row_B Col_2
moveB3 = Move Row_B Col_3
moveC1 = Move Row_C Col_1
moveC2 = Move Row_C Col_2
moveC3 = Move Row_C Col_3

-- Simulate game 1

moves1 = [moveA1]
moves2 = [moveA1, moveA2]
shouldBeFalse = isMoveAvailable moveA1 moves1
shouldBeTrue = isMoveAvailable moveA3 moves1


-- player1pkh = "player1" :: BuiltinByteString
-- player2pkh = "player2" :: BuiltinByteString


moveMadeP1A1 = MoveMade player1pkh moveA1
moveMadeP1A2 = MoveMade player1pkh moveA2
moveMadeP1A3 = MoveMade player1pkh moveA3

moveMadeP1B1 = MoveMade player1pkh moveB1
moveMadeP1B2 = MoveMade player1pkh moveB2
moveMadeP1B3 = MoveMade player1pkh moveB3

moveMadeP1C1 = MoveMade player1pkh moveC1
moveMadeP1C2 = MoveMade player1pkh moveC2
moveMadeP1C3 = MoveMade player1pkh moveC3

moveMadeP2A1 = MoveMade player2pkh moveA1
moveMadeP2A2 = MoveMade player2pkh moveA2
moveMadeP2A3 = MoveMade player2pkh moveA3

moveMadeP2B1 = MoveMade player2pkh moveB1
moveMadeP2B2 = MoveMade player2pkh moveB2
moveMadeP2B3 = MoveMade player2pkh moveB3

moveMadeP2C1 = MoveMade player2pkh moveC1
moveMadeP2C2 = MoveMade player2pkh moveC2
moveMadeP2C3 = MoveMade player2pkh moveC3

winningGameMoves = 
    [ moveMadeP1A1
    , moveMadeP2B2
    , moveMadeP1A2
    , moveMadeP2B3
    , moveMadeP1A3
    ]

gameInProgress1 = GameInProgress
    { gipPlayerOnePubKeyHash         = player1pkh
    , gipPlayerTwoPubKeyHash        = player2pkh
    , gipBetInAda                   = 50
    , gipGameMaxIntervalInSeconds   = 1
    , gipOccurredAtPosixTime        = 1000
    , gipPlayerAddressToMakeMove    = player2pkh
    , gipMovesMade                  = MovesMade []
    }
makeMoveCommand1 = MakeMoveCommand
    { mmcPlayerPubKeyHash           = player2pkh
    , mmcMove                       = moveA1
    }

makeMove1SouldBeTrue = isMoveAvailableInThisGame gameInProgress1 makeMoveCommand1

{-
DEBUG: cardano-cli address key-hash --payment-verification-key-file /home/jaco/CardanoProjects/testnet/priv/wallet/player1/player1.payment.vkey
fb21b4500aa8740c8335fc75914e96b8d66c1afc57c03ad0f98ad928
DEBUG: cardano-cli address key-hash --payment-verification-key-file /home/jaco/CardanoProjects/testnet/priv/wallet/player2/player2.payment.vkey
91f60209b232cac65d34c0584fdc33d7024de208d78e4c696fef3a63
-}


player1pkh :: BuiltinByteString
player1pkh = "fb21b4500aa8740c8335fc75914e96b8d66c1afc57c03ad0f98ad928"

player2pkh :: BuiltinByteString
player2pkh = "91f60209b232cac65d34c0584fdc33d7024de208d78e4c696fef3a63"

-- player1PubKeyHash :: PubKeyHash
-- player1PubKeyHash = PubKeyHash player1pkh


-- magic numbers
-- indexedByteString :: [Integer]
-- indexedByteString = [indexByteString player1pkh i | i <- [0..27]]

-- manager :: PubKeyHash
-- manager = PubKeyHash { getPubKeyHash = (PlutusTx.Prelude.foldr (\x y -> consByteString x y) emptyByteString indexedByteString) }
