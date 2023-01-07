import { printWalletUtxoStack } from "../app/utils.js";
import { cardanoCli } from "../previewCardanoCliJs.js";

const playerName = "player1";
const playerUtxoStack = cardanoCli.getUtxoStackForWallet(playerName);

printWalletUtxoStack(playerName, playerUtxoStack);
