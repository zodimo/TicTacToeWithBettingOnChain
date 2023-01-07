import { cardanoCli } from "../previewCardanoCliJs.js";
import { printWallet } from "../app/utils.js";

const player1Name = "player1"
const player2Name = "player2"

const player1Wallet = cardanoCli.createWallet(player1Name);
const player2Wallet = cardanoCli.createWallet(player2Name);

printWallet(player1Name, player1Wallet);
printWallet(player2Name, player2Wallet);
