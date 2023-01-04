import { cardanoCli } from "../../previewCardanoCliJs.js";
import { getTypedAlwaysSucceedScriptAddress, getTypedAlwaysSucceedScriptFile } from "../../smart-contract.js";
import { sendStartGameCommandToScriptTransaction } from "../transaction-construction/start-game-transaction.js";
import { sendCancelInitiatedGameCommandToScriptTransaction } from "../transaction-construction/cancel-initiated-game-transaction copy.js";

const player1Wallet = cardanoCli.wallet("player1");
const player2Wallet = cardanoCli.wallet("player2");
const scriptAddress = getTypedAlwaysSucceedScriptAddress();
const scriptFile = getTypedAlwaysSucceedScriptFile();

/**
 * The changes from the wins is from TX7
 */

////////////
// TX 1
////////////

const tx1UtxoId = sendStartGameCommandToScriptTransaction(player1Wallet, 5, 1, scriptAddress);

// Timeout is 1 seconds

///////////////////////////////////////////////
//      ONLY UtxoId crosses the line
//////////////////////////////////////////////

////////////
// TX 2
////////////

const tx2UtxoId = sendCancelInitiatedGameCommandToScriptTransaction(
  player1Wallet,
  scriptAddress,
  scriptFile,
  tx1UtxoId,
  [player1Wallet, player2Wallet]
);

console.log("##########################");
console.log(`The last utxos of the cancelled game : ${tx2UtxoId}`);
