{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE FlexibleContexts    #-}  --Enable flexible contexts. Implied by ImplicitParams
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE ScopedTypeVariables #-}  --Enable lexical scoping of type variables explicit introduced with forall
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax
{-# LANGUAGE TypeApplications    #-}  --Allow the use of type application syntax
{-# LANGUAGE TypeFamilies        #-}  --Allow use and definition of indexed type and data families
{-# LANGUAGE TypeOperators       #-}  --Allow the use and definition of types with operator names
{-# LANGUAGE OverloadedStrings   #-}  --Allow string to be used for bytestring

module Untyped where

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
import qualified Plutus.Script.Utils.V2.Scripts as Scripts            -- New library name for Typed Validators and some new fuctions=
import qualified Plutus.V2.Ledger.Api           as PlutusV2    
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



{-# INLINABLE mkValidator #-} 
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()


validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])  

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Address.Address
scrAddress = Address.scriptHashAddress valHash

