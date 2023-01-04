import { cardanoCli } from "../previewCardanoCliJs.js";
import { getScriptAddress, getTypedAlwaysSucceedScriptAddress, getUntypedAlwaysSucceedScriptAddress } from "../smart-contract.js";
const scriptAddress = getScriptAddress();
// const scriptAddress = getUntypedAlwaysSucceedScriptAddress();
// const scriptAddress = getTypedAlwaysSucceedScriptAddress();
console.log(`ScriptAddress: ${scriptAddress}`);
console.log(JSON.stringify(cardanoCli.getUtxoStackForAddress(scriptAddress),null,2));
