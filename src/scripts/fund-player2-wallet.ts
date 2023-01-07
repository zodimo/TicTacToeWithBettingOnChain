import { cardanoCli } from "../previewCardanoCliJs.js";
import { TransactionBuildRawOptions } from "../cardano-cli/transaction/buid-raw-options.js";
import { TransactionCalculateMinFeeOptions } from "../cardano-cli/transaction/calculate-min-fee-options.js";
import { TransactionSignOptions } from "../cardano-cli/transaction/sign-options.js";
import { TransactionSubmitOptions } from "../cardano-cli/transaction/submit-options.js";
import { TxIn, TxInParameter } from "../cardano-cli/command/transaction/build-raw/tx-in.js";
import { TxOut, TxOutParameter } from "../cardano-cli/command/transaction/build-raw/tx-out.js";
import { Fee } from "../cardano-cli/command/shared/fee.js";
import { TxToSign } from "../cardano-cli/command/transaction/sign/tx-to-sign.js";
import { SigningKeyFile } from "../cardano-cli/command/transaction/sign/signing-key-file.js";

//ensure node is running.
try {
  cardanoCli.query().tip().runCommand();
} catch (error) {
  console.log(error);
  process.exit(1);
}

// funded wallet
const player1Wallet = cardanoCli.wallet("player1");

const player2Wallet = cardanoCli.wallet("player2");
// ensure players has funds.

const lovelaceToTransfer = cardanoCli.toLovelace(1000);
let player1LovelaceChangeValue: number =
  cardanoCli.getUtxoStackForAddress(player1Wallet.paymentAddr).getLoveLaceValue() - lovelaceToTransfer;

const createTxBuildOptions: (changeInLovelace: number, fee: number) => TransactionBuildRawOptions = (
  changeInLovelace,
  fee
) => {
  return new TransactionBuildRawOptions()
    .withTxIns(
      cardanoCli.getUtxoListForAddress(player1Wallet.paymentAddr).map((utxo) => new TxInParameter(new TxIn(utxo.id)))
    )
    .withTxOuts([
      new TxOutParameter(new TxOut(player1Wallet.paymentAddr, changeInLovelace)), //convension --- change
      new TxOutParameter(new TxOut(player2Wallet.paymentAddr, lovelaceToTransfer)),
    ])
    .withFee(new Fee(fee));
};

let txDraftBodyFile = cardanoCli.transactionBuildRaw(createTxBuildOptions(player1LovelaceChangeValue, 0));

//calculate fee
let fee = cardanoCli.transactionCalculateMinFee(
  new TransactionCalculateMinFeeOptions(
    txDraftBodyFile,
    1, // inCount
    2, // outCount
    1 // witnessCounts
  )
);

//pay the fee by subtracting it from the sender utxo
player1LovelaceChangeValue = player1LovelaceChangeValue - fee;

//create final transaction
let txBodyFile = cardanoCli.transactionBuildRaw(createTxBuildOptions(player1LovelaceChangeValue, fee));

//sign the transaction
let txSignedFile = cardanoCli.transactionSign(
  new TransactionSignOptions(TxToSign.txBodyFile(txBodyFile), [
    new SigningKeyFile(player1Wallet.keys.payment.signingKeyFile),
  ])
);

//broadcast transaction
let txHash = cardanoCli.transactionSubmit(new TransactionSubmitOptions(txSignedFile));
console.log("TxHash: " + txHash);
