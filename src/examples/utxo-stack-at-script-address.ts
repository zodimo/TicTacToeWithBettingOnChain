import { cardanoCli } from "../previewCardanoCliJs.js";
import { getScriptAddress } from "../smart-contract.js";
const scriptAddress = getScriptAddress();
console.log(cardanoCli.getUtxoStackFor(scriptAddress));
