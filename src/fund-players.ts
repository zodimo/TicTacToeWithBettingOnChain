import cardanocliJs from "./previewCardanoCliJs.js";
import { createWallet } from "./utils.js";
import { TxIn, TxOut } from "./models.js";
import CardanocliJs from "cardanocli-js";
//ensure node is running.
try {
  const ctip = cardanocliJs.queryTip();
} catch (error) {
  console.log(error);
  process.exit(1);
}

// const player1=createWallet("player1");
// const player2=createWallet("player2");

// funded wallet
const jaco = cardanocliJs.wallet("jaco");
const player1 = cardanocliJs.wallet("player1");
const player2 = cardanocliJs.wallet("player2");
// ensure players has funds.

console.log(cardanocliJs.queryUtxo(jaco.paymentAddr));

export interface Utxo {
  txHash: string;
  txId: number;
  value: any;
}

const txInfo = {
  txIn: cardanocliJs
    .queryUtxo(jaco.paymentAddr)
    .map((utxo: Utxo) => TxIn.fromUtxo(utxo)),
  txOut: [
    new TxOut(jaco.paymentAddr, {
      lovelace: jaco.balance().value.lovelace - cardanocliJs.toLovelace(600),
    }), //value going back to sender
    new TxOut(player1.paymentAddr, { lovelace: cardanocliJs.toLovelace(300) }), //value going to player1
    new TxOut(player2.paymentAddr, { lovelace: cardanocliJs.toLovelace(300) }), //value going to player2
  ],
};

let raw = cardanocliJs.transactionBuildRaw(txInfo);

console.log(raw);

//calculate fee
let fee = cardanocliJs.transactionCalculateMinFee({
    ...txInfo,
    txBody: raw,
    witnessCount: 1,
  });

//pay the fee by subtracting it from the sender utxo
txInfo.txOut[0].value.lovelace -= fee;

//create final transaction
let tx = cardanocliJs.transactionBuildRaw({ ...txInfo, fee });

//sign the transaction
let txSigned = cardanocliJs.transactionSign({
  txBody: tx,
  signingKeys: [jaco.payment.skey],
});


//broadcast transaction
let txHash = cardanocliJs.transactionSubmit(txSigned);
console.log("TxHash: " + txHash);

