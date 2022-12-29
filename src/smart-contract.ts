import { cardanoCli } from "./previewCardanoCliJs.js";
import path from "path";
import { fileURLToPath } from "url";
import { PaymentAddressBuildOptions } from "./cardano-cli/address-build-options.js";
import { PaymentComponent } from "./cardano-cli/command/address/build.js";

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
    new PaymentAddressBuildOptions(PaymentComponent.scriptFile(getScriptFile()))
  );
  return cardanoCli.scriptWallet(scriptWalletName).paymentAddr;
};

// console.log(getScriptAddress());
