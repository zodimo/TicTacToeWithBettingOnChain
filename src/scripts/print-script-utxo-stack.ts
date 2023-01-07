import { printWalletUtxoStack } from "../app/utils.js";
import { cardanoCli } from "../previewCardanoCliJs.js";

const scriptWalletName = "ticTacToeScript";
const player1UtxoStack = cardanoCli.getUtxoStackForScriptWallet(scriptWalletName);

printWalletUtxoStack(scriptWalletName, player1UtxoStack);
