{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE FlexibleContexts    #-}  --Enable flexible contexts. Implied by ImplicitParams
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE ScopedTypeVariables #-}  --Enable lexical scoping of type variables explicit introduced with forall
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax
{-# LANGUAGE TypeApplications    #-}  --Allow the use of type application syntax
{-# LANGUAGE TypeFamilies        #-}  --Allow use and definition of indexed type and data families
{-# LANGUAGE TypeOperators       #-}  --Allow the use and definition of types with operator names

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

-- custom data types

data Row = Row_A| Row_B | Row_C deriving Show
data Column = Col_1 | Col_2 | Col_3 deriving Show
data Move = Move Row Column deriving Show

data GameStateDatum = GameStateDatum
    { oWallet :: PaymentPubKeyHash
    , xWallet :: Maybe PaymentPubKeyHash
    , deadline    :: POSIXTime
    , bet :: Integer
    , moves :: Maybe [Move]
    } deriving Show


-- loadWorldStateFromDatum :: GameStateDatum -> WorldState 

-- template haskell to make instance of data for custom datatypes.
PlutusTx.unstableMakeIsData ''GameStateDatum
PlutusTx.unstableMakeIsData ''Move
PlutusTx.unstableMakeIsData ''Row
PlutusTx.unstableMakeIsData ''Column


{-# INLINABLE mkValidator #-} -- Everything that its supposed to run in on-chain code need this pragma
 -- Datum -- Redeemer -- ScriptContext
mkValidator :: GameStateDatum -> () -> PlutusV2.ScriptContext -> Bool   -- the value of this function is on its sideeffects
mkValidator _ _ _ = True


data Game
instance Scripts.ValidatorTypes Game where
    type instance DatumType Game = GameStateDatum
    type instance RedeemerType Game = ()


typedValidator :: Scripts.TypedValidator Game
typedValidator = Scripts.mkTypedValidator @Game
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator @GameStateDatum @()

    

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = Scripts.validatorAddress typedValidator

