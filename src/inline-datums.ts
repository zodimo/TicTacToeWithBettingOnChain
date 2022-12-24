import cardanocliJs from "./previewCardanoCliJs.js";
import * as a from "@emurgo/cardano-serialization-lib-nodejs";
import { getScriptAddress } from "./smart-contract.js";
import { Transaction, Utxo } from "cardanocli-js";
import { TxIn, TxOut } from "./models.js";
import { StartGameData } from "./emurgo-datum";
//ensure node is running.
try {
  const ctip = cardanocliJs.queryTip();
} catch (error) {
  console.log(error);
  process.exit(1);
}

const scriptAddress = getScriptAddress();
const jacoWallet = cardanocliJs.wallet("jaco");

// @see https://github.com/input-output-hk/Vasil-testnet/blob/main/extra-example.md
// @see https://github.com/input-output-hk/Vasil-testnet/blob/main/inline-datums-cip-32.md

const startGameData = new StartGameData(10, 30);




  // let raw = cardanocliJs.transactionBuildRaw(txInfo);