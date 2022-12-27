import { cardanoCli } from "../previewCardanoCliJs.js";
import { StartGameData } from "../emurgo-datum.js";
import {
  SigningKeyFiles,
  TransactionBuildRawOptions,
  TransactionCalculateMinFeeOptions,
  TransactionSignOptions,
  TransactionSubmitOptions,
  TxInParameter,
  TxOut,
  TxOutDatum,
  TxOutParameter,
  TxToSign,
} from "../cardano-cli/transaction.js";
import { getScriptAddress } from "../smart-contract.js";

const startGameDatum = new StartGameData(50);

// funded wallet
const jacoWallet = cardanoCli.wallet("jaco");
// console.log(jaco);
// console.log(startGameDatum.toScriptDataJson());

// console.log(cardanoCli.queryTip())

const paymentAddressAsInput: (paymentAddress: string) => TxInParameter[] = (
  paymentAddress
) => {
  return cardanoCli
    .queryUtxo(paymentAddress)
    .map((utxo) => new TxInParameter(utxo.id));
};

const scriptAddress = getScriptAddress();

const sendLovelaceToScript = cardanoCli.toLovelace(5);
//calculate change without fees to start with
let jacoWalletChangeInLovelace =
  cardanoCli.getUtxoStackFor(jacoWallet.paymentAddr).getLoveLaceValue() -
  sendLovelaceToScript;

const transactionBuildRawOptions = new TransactionBuildRawOptions(
  paymentAddressAsInput(jacoWallet.paymentAddr),
  [
    new TxOutParameter(
      new TxOut(jacoWallet.paymentAddr, jacoWalletChangeInLovelace)
    ),
    new TxOutParameter(
      new TxOut(scriptAddress, sendLovelaceToScript),
      TxOutDatum.inlineValue(startGameDatum.toScriptDataJson())
    ),
  ]
);
//draft transaction
const draftTransactionBodyFile = cardanoCli.transactionBuildRaw(
  transactionBuildRawOptions
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
transactionBuildRawOptions.txOut[0].setLovelaveValue(
  jacoWalletChangeInLovelace - fee
);
transactionBuildRawOptions.fee = fee;

// sign transaction
const txBodyToSign = cardanoCli.transactionBuildRaw(transactionBuildRawOptions);
const signedTransactionFile = cardanoCli.transactionSign(
  new TransactionSignOptions(
    TxToSign.txBodyFile(txBodyToSign),
    new SigningKeyFiles([jacoWallet.keys.payment.signingKeyFile])
  )
);

//broadcast transaction
let txHash = cardanoCli.transactionSubmit(
  new TransactionSubmitOptions(signedTransactionFile)
);
console.log("TxHash: " + txHash);

const utxoId = `${txHash}#1`;

//wait for transaction to arrive
cardanoCli.waitForUtxoAtPaymentAddress(scriptAddress, utxoId);
console.log(`Utxo found at paymentAddess ${scriptAddress}}`);
