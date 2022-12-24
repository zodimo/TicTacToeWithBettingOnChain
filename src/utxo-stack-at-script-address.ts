import { cardanoCli } from "./new-cardano-cli.js";
import { getScriptAddress } from "./smart-contract.js";
const scriptAddress = getScriptAddress();
console.log(cardanoCli.getUtxoStackFor(scriptAddress));
