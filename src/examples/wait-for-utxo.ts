import { UtxoId } from "../cardano-cli/utxo-id.js";
import { cardanoCli } from "../previewCardanoCliJs.js";
import { getScriptAddress } from "../smart-contract.js";
const scriptAddress = getScriptAddress();
const utxoId = UtxoId.fromString(
  "1e7c975f501c222b15fc46f9d04f8d47f7f1865dcbe1fc058f9e078054b4f770#1"
);
cardanoCli.waitForUtxoAtPaymentAddress(scriptAddress, utxoId);
