import { cardanoCli } from "../previewCardanoCliJs.js";
import { createWallet } from "../create-wallet.js";
import { TransactionBuildRawOptions } from "../cardano-cli/transaction/buid-raw.js";
import { TxInParameter } from "../cardano-cli/transaction/build-raw/tx-in.js";
import { TxOut, TxOutParameter } from "../cardano-cli/transaction/build-raw/tx-out.js";
import { TransactionCalculateMinFeeOptions } from "../cardano-cli/transaction/calculate-min-fee.js";
import { SigningKeyFiles, TransactionSignOptions, TxToSign } from "../cardano-cli/transaction/sign.js";
import { TransactionSubmitOptions } from "../cardano-cli/transaction/submit.js";

//ensure node is running.
try {
  const ctip = cardanoCli.queryTip();
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

// console.log(cardanoCli.queryUtxo(jaco.paymentAddr));

const lovaceToTransfer=cardanoCli.toLovelace(300);
let jacoLovelaceChangeValue: number =
  cardanoCli.getUtxoStackFor(jaco.paymentAddr).getLoveLaceValue() - lovaceToTransfer - lovaceToTransfer;

const txBuildOptions: TransactionBuildRawOptions =
  new TransactionBuildRawOptions(
    cardanoCli
      .queryUtxo(jaco.paymentAddr)
      .map((utxo) => new TxInParameter(utxo.id.toString())),
    [
      new TxOutParameter(
        new TxOut(
          jaco.paymentAddr,
          cardanoCli.toLovelace(jacoLovelaceChangeValue)
        )
      ), //convension --- change
      new TxOutParameter(
        new TxOut(player1.paymentAddr, lovaceToTransfer)
      ),
      new TxOutParameter(
        new TxOut(player2.paymentAddr, lovaceToTransfer)
      ),
    ]
  );

let txDraftBodyFile = cardanoCli.transactionBuildRaw(txBuildOptions);

//calculate fee
let fee = cardanoCli.transactionCalculateMinFee(
  new TransactionCalculateMinFeeOptions(
    txDraftBodyFile,
    1, // inCount
    3, // outCoutn
    1 // withnessCounts
  )
);

// //pay the fee by subtracting it from the sender utxo
txBuildOptions.txOut[0].setLovelaveValue(jacoLovelaceChangeValue - fee);
txBuildOptions.fee = fee;

//create final transaction
let txBodyFile = cardanoCli.transactionBuildRaw(txBuildOptions);

//sign the transaction
let txSignedFile = cardanoCli.transactionSign(
  new TransactionSignOptions(
    TxToSign.txBodyFile(txBodyFile),
    new SigningKeyFiles([jaco.keys.payment.signingKeyFile])
  )
);

//broadcast transaction
let txHash = cardanoCli.transactionSubmit(
  new TransactionSubmitOptions(txSignedFile)
);
console.log("TxHash: " + txHash);
