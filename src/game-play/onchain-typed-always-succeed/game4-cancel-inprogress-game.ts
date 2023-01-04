import { cardanoCli } from "../../previewCardanoCliJs.js";
import { getTypedAlwaysSucceedScriptAddress, getTypedAlwaysSucceedScriptFile } from "../../smart-contract.js";
import { sendStartGameCommandToScriptTransaction } from "../transaction-construction/start-game-transaction.js";
import { sendJoinGameCommandToScriptTransaction } from "../transaction-construction/join-game-transaction.js";
import { sendCancelInProgressGameCommandToScriptTransaction } from "../transaction-construction/cancel-in-progress-game-transaction copy 2.js";

const player1Wallet = cardanoCli.wallet("player1");
const player2Wallet = cardanoCli.wallet("player2");
const scriptAddress = getTypedAlwaysSucceedScriptAddress();
const scriptFile = getTypedAlwaysSucceedScriptFile();

////////////
// TX 1
////////////

const tx1UtxoId = sendStartGameCommandToScriptTransaction(player1Wallet, 5, 1, scriptAddress);

///////////////////////////////////////////////
//      ONLY UtxoId crosses the line
//////////////////////////////////////////////

////////////
// TX 2
////////////

const tx2UtxoId = sendJoinGameCommandToScriptTransaction(player2Wallet, scriptAddress, scriptFile, tx1UtxoId);

///////////////////////////////////////////////
//      ONLY UtxoId crosses the line
//////////////////////////////////////////////

////////////
// TX 3
////////////

// Cancel game

const tx3UtxoId = sendCancelInProgressGameCommandToScriptTransaction(
  player2Wallet,
  scriptAddress,
  scriptFile,
  tx2UtxoId,
  [player1Wallet, player2Wallet]
);
console.log("##########################");
console.log(`The last utxo of the cancelled game : ${tx3UtxoId}`);
