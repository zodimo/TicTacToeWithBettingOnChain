import { cardanoCli } from "./new-cardano-cli.js";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { PaymentAddressBuildOptions, PaymentVerification } from "./cardano-cli/address.js";

export const getScriptAddress = () => {
  const __filename = fileURLToPath(import.meta.url);
  const __dirname = path.dirname(__filename);

  // get script address
  const scriptFile = __dirname + "/../testnet/ticTacToe.plutus";
 
  const scriptWalletName = "ticTacToeScript";
  cardanoCli.paymentAddressBuild(scriptWalletName, 
    new PaymentAddressBuildOptions(
      PaymentVerification.scriptFile(scriptFile)
    ));
  return cardanoCli.scriptWallet(scriptWalletName).paymentAddr;
};

// console.log(getScriptAddress());
