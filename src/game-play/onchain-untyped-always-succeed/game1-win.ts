import { Column, Move, Row } from "../../app/game-data.js";

import { cardanoCli } from "../../previewCardanoCliJs.js";
import { getUntypedAlwaysSucceedScriptAddress, getUntypedAlwaysSucceedScriptFile } from "../../smart-contract.js";
import { sendStartGameCommandToScriptTransaction } from "../transaction-construction/start-game-transaction.js";
import { sendJoinGameCommandToScriptTransaction } from "../transaction-construction/join-game-transaction.js";
import { sendMakeMoveCommandToScriptTransaction } from "../transaction-construction/make-move-transaction.js";
import { sendClaimWinCommandToScriptTransaction } from "../transaction-construction/claim-win-transaction.js";

const player1Wallet = cardanoCli.wallet("player1");
const player2Wallet = cardanoCli.wallet("player2");
const scriptAddress = getUntypedAlwaysSucceedScriptAddress();
const scriptFile = getUntypedAlwaysSucceedScriptFile();

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
// fake game start with player 2 as O and move A 1

//    1   2   3
// A _O_|___|___
// B ___|___|___
// C    |   |

const tx3Move = new Move(Row.ROW_A, Column.Col_1);
const tx3UtxoId = sendMakeMoveCommandToScriptTransaction(player2Wallet, tx3Move, scriptAddress, scriptFile, tx2UtxoId);

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 4
// Player 1 Move B 2
//    1   2   3

// A _O_|___|___
// B ___|_X_|___
// C    |   |

const tx4Move = new Move(Row.ROW_B, Column.Col_2);
const tx4UtxoId = sendMakeMoveCommandToScriptTransaction(player1Wallet, tx4Move, scriptAddress, scriptFile, tx3UtxoId);
// const tx4UtxoFromStack = getUtxoFromScriptAddress(tx4UtxoId, scriptAddress);
// console.log(JSON.stringify(tx4UtxoFromStack, null, 2));

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 5
// Player 2 Move A 2

// A _O_|_O_|___
// B ___|_X_|___
// C    |   |

const tx5Move = new Move(Row.ROW_A, Column.Col_2);
const tx5UtxoId = sendMakeMoveCommandToScriptTransaction(player2Wallet, tx5Move, scriptAddress, scriptFile, tx4UtxoId);
// const tx5UtxoFromStack = getUtxoFromScriptAddress(tx5UtxoId, scriptAddress);
// console.log(JSON.stringify(tx5UtxoFromStack, null, 2));

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 6
// Player 1 Move B 3

// A _O_|_O_|___
// B ___|_X_|_X_
// C    |   |

const tx6Move = new Move(Row.ROW_B, Column.Col_3);
const tx6UtxoId = sendMakeMoveCommandToScriptTransaction(player1Wallet, tx6Move, scriptAddress, scriptFile, tx5UtxoId);
// const tx6UtxoFromStack = getUtxoFromScriptAddress(tx6UtxoId, scriptAddress);
// console.log(JSON.stringify(tx6UtxoFromStack, null, 2));

// ///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

// TX 7
// Player 2 Move A 3

// A _O_|_O_|_O_
// B ___|_X_|_X_
// C    |   |

const tx7Move = new Move(Row.ROW_A, Column.Col_3);
const tx7UtxoId = sendMakeMoveCommandToScriptTransaction(player2Wallet, tx7Move, scriptAddress, scriptFile, tx6UtxoId);
// const tx7UtxoFromStack = getUtxoFromScriptAddress(tx7UtxoId, scriptAddress);
// console.log(JSON.stringify(tx7UtxoFromStack, null, 2));

///////////////////////////////////////////////
//      ONLY ScriptData crosses the line
//////////////////////////////////////////////

//any player can call claim win :) to pay the fees.
// TX 8 Claim Win
const tx8UtxoId = sendClaimWinCommandToScriptTransaction(player2Wallet, scriptAddress, scriptFile, tx7UtxoId, [
  player1Wallet,
  player2Wallet,
]);
console.log("##########################");
console.log(`The last utxo of the winning game : ${tx8UtxoId}`);
