import {
  TxOut,
  TxOutParameter,
} from "../cardano-cli/command/transaction/build/tx-out.js";
import { RequiredSigner } from "../cardano-cli/command/transaction/build/required-signer.js";
import {
  TxIn,
  TxInAdditional,
  TxInAlternativeFactory,
  TxInDatum,
  TxInParameter,
  TxInRedeemer,
  TxInScriptAdditional,
} from "../cardano-cli/command/transaction/build/tx-in.js";
import { TransactionBuildOptions } from "../cardano-cli/transaction-build-options.js";
import { UtxoId } from "../cardano-cli/utxo-id.js";
import { UtxoStack } from "../cardano-cli/utxo.js";
import { cardanoCli } from "../previewCardanoCliJs.js";
import { getScriptAddress, getScriptFile } from "../smart-contract.js";
import { PlutusScriptDataJsonSchema, UnitData } from "../emurgo-datum.js";

const scriptAddress = getScriptAddress();
const utxoStackAtScriptAddress =
  cardanoCli.getUtxoStackForAddress(scriptAddress);
const scriptFile = getScriptFile();

//destination wallet
const jacoWallet = cardanoCli.wallet("jaco");
const jacoWalletUtxoStack = cardanoCli.getUtxoStackForAddress(
  jacoWallet.paymentAddr
);
const plutusSchema = PlutusScriptDataJsonSchema.ScriptDataJsonDetailedSchema;
const scriptDatumValue = new UnitData().toScriptDataJson(plutusSchema);
const scriptRedeemerValue = new UnitData().toScriptDataJson(plutusSchema);

const scriptUtxoStack = new UtxoStack(
  cardanoCli.getUtxoListForAddress(scriptAddress).filter((utxo) => {
    //with inline datum
    return !!utxo.inlineDatum;
  })
);

//filter input utxos to only the ones containing inline datum..
const scriptUtxoStackAsInput: (utxoStack: UtxoStack) => TxInParameter[] = (
  utxoStack
) => {
  return [
    utxoStack.utxos.map((utxo) => {
      return new TxInParameter(
        new TxIn(utxo.id), // tx-in
        new TxInAdditional(
          TxInAlternativeFactory.txInScriptFile(scriptFile),
          new TxInScriptAdditional()
            .withTxInDatum(TxInDatum.value(scriptDatumValue))
            .withTxInRedeemer(TxInRedeemer.value(scriptRedeemerValue))
        )
      );
    })[0],
  ];
};

//calculate output without fees to start with
const outputValueInLovelace = scriptUtxoStack.getLoveLaceValue();

console.log(scriptUtxoStack);

const transactionBuildOptions = new TransactionBuildOptions();
transactionBuildOptions.withTxIns(scriptUtxoStackAsInput(scriptUtxoStack));
transactionBuildOptions.withRequiredSigner(
  RequiredSigner.file(jacoWallet.keys.payment.signingKeyFile)
);
transactionBuildOptions.withTxInCollateral(
  jacoWalletUtxoStack.utxos[0].id.toString()
);
transactionBuildOptions.withTxOut(
  new TxOutParameter(new TxOut(jacoWallet.paymentAddr, outputValueInLovelace))
);
transactionBuildOptions.withChangeAddress(jacoWallet.paymentAddr);

//draft transaction
const draftTransactionBodyFile = cardanoCli.transactionBuild(
  transactionBuildOptions
);

console.log(draftTransactionBodyFile);
// // calculate transaction fees
// const fee = cardanoCli.transactionCalculateMinFee(
//   new TransactionCalculateMinFeeOptions(
//     draftTransactionBodyFile, //txBodyFile
//     scriptUtxoStack.utxos.length, // inputCount
//     1, // outputCount
//     1 //witnessCount
//   )
// );

// //update fee
// transactionBuildRawOptions.txOut[0].setLovelaveValue(
//   outputValueInLovelace - fee
// );
// transactionBuildRawOptions.fee = fee;

// // sign transaction
// const txBodyToSign = cardanoCli.transactionBuildRaw(transactionBuildRawOptions);
// const signedTransactionFile = cardanoCli.transactionSign(
//   new TransactionSignOptions(
//     TxToSign.txBodyFile(txBodyToSign),
//     new SigningKeyFiles([jacoWallet.keys.payment.signingKeyFile])
//   )
// );

// //broadcast transaction
// let txHash = cardanoCli.transactionSubmit(
//   new TransactionSubmitOptions(signedTransactionFile)
// );
// console.log("TxHash: " + txHash);

// const utxoId = new UtxoId(txHash, 1);

// //wait for transaction to arrive
// cardanoCli.waitForUtxoAtPaymentAddress(jacoWallet.paymentAddr, utxoId);
// console.log(`Utxo found at paymentAddess ${scriptAddress}}`);
