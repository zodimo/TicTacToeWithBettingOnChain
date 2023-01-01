import { cardanoCli } from "../previewCardanoCliJs.js";
import { getScriptAddress } from "../smart-contract.js";
const scriptAddress = getScriptAddress();
console.log(`ScriptAddress: ${scriptAddress}`);
console.log(JSON.stringify(cardanoCli.getUtxoStackForAddress(scriptAddress),null,2));
