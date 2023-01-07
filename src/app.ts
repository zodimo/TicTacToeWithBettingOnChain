import { PaymentVerificationKey } from "./cardano-cli/command/address/key-hash.js";
import { DataBytes, ScriptDataJsonSchema } from "./cardano-cli/script-data.js";
import { cardanoCli } from "./previewCardanoCliJs.js";

/**
 * Builder pattern..
 *
 *
 */

// console.log(cardanoCli.getCurrentSlotFromQueryTip());
const player1Wallet = cardanoCli.wallet("player1");
const player2Wallet = cardanoCli.wallet("player2");
console.log(cardanoCli.pubKeyHashFromVerificationKeyFile(player1Wallet.keys.payment.verificationKeyFile));
console.log(cardanoCli.pubKeyHashFromVerificationKeyFile(player2Wallet.keys.payment.verificationKeyFile));

console.log(DataBytes.fromHexString(cardanoCli.pubKeyHashFromVerificationKeyFile(player2Wallet.keys.payment.verificationKeyFile)).toScriptDataJson(ScriptDataJsonSchema.ScriptDataJsonDetailedSchema));


