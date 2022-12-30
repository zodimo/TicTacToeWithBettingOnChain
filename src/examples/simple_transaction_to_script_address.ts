import { cardanoCli } from "../previewCardanoCliJs.js";
import { PlutusScriptDataJsonSchema, StartGameData } from "../emurgo-datum.js";
import { getScriptAddress } from "../smart-contract.js";
import { TxIn, TxInParameter } from "../cardano-cli/command/transaction/build-raw/tx-in.js";
import { TransactionBuildRawOptions } from "../cardano-cli/transaction/buid-raw-options.js";
import { TxOut, TxOutDatum, TxOutParameter } from "../cardano-cli/command/transaction/build-raw/tx-out.js";
import { TransactionCalculateMinFeeOptions } from "../cardano-cli/transaction/calculate-min-fee-options.js";
import { Fee } from "../cardano-cli/command/shared/fee.js";
import { TransactionSignOptions } from "../cardano-cli/transaction/sign-options.js";
import { TxToSign } from "../cardano-cli/command/transaction/sign/tx-to-sign.js";
import { SigningKeyFile } from "../cardano-cli/command/transaction/sign/signing-key-file.js";
import { TransactionSubmitOptions } from "../cardano-cli/transaction/submit-options.js";
import { UtxoId } from "../cardano-cli/utxo-id.js";

const startGameDatum = new StartGameData(50);

// funded wallet
const jacoWallet = cardanoCli.wallet("jaco");
// console.log(jaco);
// console.log(startGameDatum.toScriptDataJson());

// console.log(cardanoCli.queryTip())

const paymentAddressAsInput: (paymentAddress: string) => TxInParameter[] = (paymentAddress) => {
  return cardanoCli.getUtxoListForAddress(paymentAddress).map((utxo) => new TxInParameter(new TxIn(utxo.id)));
};

const scriptAddress = getScriptAddress();

const sendLovelaceToScript = cardanoCli.toLovelace(5);
//calculate change without fees to start with
let jacoWalletChangeInLovelace =
  cardanoCli.getUtxoStackForAddress(jacoWallet.paymentAddr).getLoveLaceValue() - sendLovelaceToScript;

const createTransactionBuildRawOptionsForChangeAndFee: (
  changeInLovelace: number,
  fee: number
) => TransactionBuildRawOptions = (changeInLovelace, fee) => {
  return new TransactionBuildRawOptions()
    .withTxIns(paymentAddressAsInput(jacoWallet.paymentAddr))
    .withTxOuts([
      new TxOutParameter(new TxOut(jacoWallet.paymentAddr, changeInLovelace)),
      new TxOutParameter(new TxOut(scriptAddress, sendLovelaceToScript)),
    ])
    .withFee(new Fee(fee));
};

//draft transaction
const draftTransactionBodyFile = cardanoCli.transactionBuildRaw(
  createTransactionBuildRawOptionsForChangeAndFee(jacoWalletChangeInLovelace, 0)
);

// calculate transaction fees
const fee = cardanoCli.transactionCalculateMinFee(
  new TransactionCalculateMinFeeOptions(
    draftTransactionBodyFile, //txBodyFile
    1, // inputCount
    2, // outputCount
    1 //witnessCount
  )
);

// subtract fee from change
jacoWalletChangeInLovelace = jacoWalletChangeInLovelace - fee;

// sign transaction
const txBodyToSign = cardanoCli.transactionBuildRaw(
  createTransactionBuildRawOptionsForChangeAndFee(jacoWalletChangeInLovelace, fee)
);
const signedTransactionFile = cardanoCli.transactionSign(
  new TransactionSignOptions(TxToSign.txBodyFile(txBodyToSign), [
    new SigningKeyFile(jacoWallet.keys.payment.signingKeyFile),
  ])
);

//broadcast transaction
let txHash = cardanoCli.transactionSubmit(new TransactionSubmitOptions(signedTransactionFile));
console.log("TxHash: " + txHash);

const utxoId = new UtxoId(txHash, 1);

//wait for transaction to arrive
cardanoCli.waitForUtxoAtPaymentAddress(scriptAddress, utxoId);
console.log(`Utxo found at paymentAddess ${scriptAddress}}`);
