import { cardanoCli } from "../previewCardanoCliJs.js";
import { getScriptAddress, getUntypedAlwaysSucceedScriptAddress } from "../smart-contract.js";
// const scriptAddress = getScriptAddress();
const scriptAddress = getUntypedAlwaysSucceedScriptAddress();
console.log(`ScriptAddress: ${scriptAddress}`);
console.log(JSON.stringify(cardanoCli.getUtxoStackForAddress(scriptAddress),null,2));
