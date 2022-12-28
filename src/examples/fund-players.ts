import { cardanoCli } from "../previewCardanoCliJs.js";
import { createWallet } from "../create-wallet.js";
import { TransactionBuildRawOptions } from "../cardano-cli/transaction/buid-raw-options.js";
import { TransactionCalculateMinFeeOptions } from "../cardano-cli/transaction/calculate-min-fee-options.js";
import { TransactionSignOptions } from "../cardano-cli/transaction/sign-options.js";
import { TransactionSubmitOptions } from "../cardano-cli/transaction/submit-options.js";
import {
  TxIn,
  TxInParameter,
} from "../cardano-cli/command/transaction/build-raw/tx-in.js";
import {
  TxOut,
  TxOutParameter,
} from "../cardano-cli/command/transaction/build-raw/tx-out.js";
import { Fee } from "../cardano-cli/command/shared/fee.js";
import { TxToSign } from "../cardano-cli/command/transaction/sign/tx-to-sign.js";
import { SigningKeyFile } from "../cardano-cli/command/transaction/sign/signing-key-file.js";

//ensure node is running.
try {
  const ctip = cardanoCli.query().tip().runCommand();
} catch (error) {
  console.log(error);
  process.exit(1);
}

// const player1=createWallet("player1");
// const player2=createWallet("player2");

// funded wallet
const jaco = cardanoCli.wallet("jaco");
const player1 = cardanoCli.wallet("player1");
const player2 = cardanoCli.wallet("player2");
// ensure players has funds.

// console.log(cardanoCli.getUtxoListForAddress(jaco.paymentAddr));

const lovaceToTransfer = cardanoCli.toLovelace(300);
let jacoLovelaceChangeValue: number =
  cardanoCli.getUtxoStackForAddress(jaco.paymentAddr).getLoveLaceValue() -
  lovaceToTransfer -
  lovaceToTransfer;

const createTxBuildOptions: (
  changeInLovelace: number,
  fee: number
) => TransactionBuildRawOptions = (changeInLovelace, fee) => {
  return new TransactionBuildRawOptions(
    cardanoCli
      .getUtxoListForAddress(jaco.paymentAddr)
      .map((utxo) => new TxInParameter(new TxIn(utxo.id))),
    [
      new TxOutParameter(
        new TxOut(jaco.paymentAddr, changeInLovelace)
      ), //convension --- change
      new TxOutParameter(new TxOut(player1.paymentAddr, lovaceToTransfer)),
      new TxOutParameter(new TxOut(player2.paymentAddr, lovaceToTransfer)),
    ],
    new Fee(fee)
  );
};

let txDraftBodyFile = cardanoCli.transactionBuildRaw(
  createTxBuildOptions(jacoLovelaceChangeValue, 0)
);

//calculate fee
let fee = cardanoCli.transactionCalculateMinFee(
  new TransactionCalculateMinFeeOptions(
    txDraftBodyFile,
    1, // inCount
    3, // outCount
    1 // witnessCounts
  )
);

//pay the fee by subtracting it from the sender utxo
jacoLovelaceChangeValue = jacoLovelaceChangeValue - fee;

//create final transaction
let txBodyFile = cardanoCli.transactionBuildRaw(
  createTxBuildOptions(jacoLovelaceChangeValue, fee)
);

//sign the transaction
let txSignedFile = cardanoCli.transactionSign(
  new TransactionSignOptions(TxToSign.txBodyFile(txBodyFile), [
    new SigningKeyFile(jaco.keys.payment.signingKeyFile),
  ])
);

//broadcast transaction
let txHash = cardanoCli.transactionSubmit(
  new TransactionSubmitOptions(txSignedFile)
);
console.log("TxHash: " + txHash);
