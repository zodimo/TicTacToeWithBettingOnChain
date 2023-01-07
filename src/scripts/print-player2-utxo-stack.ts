import { printWalletUtxoStack } from "../app/utils.js";
import { cardanoCli } from "../previewCardanoCliJs.js";

const playerName = "player2";
const playerUtxoStack = cardanoCli.getUtxoStackForWallet(playerName);

printWalletUtxoStack(playerName, playerUtxoStack);
