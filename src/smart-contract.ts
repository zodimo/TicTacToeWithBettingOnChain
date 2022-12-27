import { cardanoCli } from "./previewCardanoCliJs.js";
import path from "path";
import { fileURLToPath } from "url";
import {
  PaymentAddressBuildOptions,
  PaymentVerification,
} from "./cardano-cli/address.js";

export const getScriptFile = () => {
  const __filename = fileURLToPath(import.meta.url);
  const __dirname = path.dirname(__filename);

  // get script address
  return __dirname + "/../testnet/ticTacToe.plutus";
};

export const getScriptAddress = () => {
  const scriptWalletName = "ticTacToeScript";
  cardanoCli.paymentAddressBuild(
    scriptWalletName,
    new PaymentAddressBuildOptions(
      PaymentVerification.scriptFile(getScriptFile())
    )
  );
  return cardanoCli.scriptWallet(scriptWalletName).paymentAddr;
};

// console.log(getScriptAddress());
