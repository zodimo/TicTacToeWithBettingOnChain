import { printWallet } from "../app/utils.js";
import { cardanoCli } from "../previewCardanoCliJs.js";

const player1Name = "player1";
const player2Name = "player2";

const player1Wallet = cardanoCli.wallet(player1Name);
const player2Wallet = cardanoCli.wallet(player2Name);

printWallet(player1Name, player1Wallet);
printWallet(player2Name, player2Wallet);
