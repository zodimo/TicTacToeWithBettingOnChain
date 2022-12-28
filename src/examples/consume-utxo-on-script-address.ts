import { UtxoId } from "../cardano-cli/utxo-id.js";
import { UtxoStack } from "../cardano-cli/utxo.js";
import { cardanoCli } from "../previewCardanoCliJs.js";
import { getScriptAddress } from "../smart-contract.js";

const scriptAddress = getScriptAddress();
const utxoStackAtScriptAddress = cardanoCli.getUtxoStackForAddress(scriptAddress);

//destination wallet
const jacoWallet = cardanoCli.wallet("jaco");

const scriptUtxoStack = new UtxoStack(
  cardanoCli.getUtxoListForAddress(scriptAddress).filter((utxo) => {
    //with inline datum
    return !!utxo.inlineDatum;
  })
);



// //filter input utxos to only the ones containing inline datum..
// const scriptUtxoStackAsInput: (utxoStack: UtxoStack) => TxInParameter[] = (
//   utxoStack
// ) => {
//   return utxoStack.utxos.map((utxo) => {

//     return new TxInParameter(
//       utxo.id.toString(), // tx-in
//       new TxInSpending(
//         true, // isSpendingPlutusScriptV2

//       )
//       );
//   });
// };

//calculate output without fees to start with
const outputValueInLovelace = scriptUtxoStack.getLoveLaceValue();

console.log(scriptUtxoStack);



// const transactionBuildRawOptions = new TransactionBuildRawOptions(
//     //filtered script utxos
//     utxoStackAsInput(scriptUtxoStack),
//   [
//     new TxOutParameter(
//       new TxOut(jacoWallet.paymentAddr, outputValueInLovelace)
//     ),
//   ]
// );

// //draft transaction
// const draftTransactionBodyFile = cardanoCli.transactionBuildRaw(
//   transactionBuildRawOptions
// );

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
