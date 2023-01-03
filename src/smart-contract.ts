import { cardanoCli } from "./previewCardanoCliJs.js";
import path from "path";
import { fileURLToPath } from "url";
import { PaymentAddressBuildOptions } from "./cardano-cli/address-build-options.js";
import { PaymentComponent } from "./cardano-cli/command/address/build.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

export const getScriptFile = () => {
  // get script address
  return __dirname + "/../testnet/ticTacToe.plutus";
};

export const getUntypedScriptFile = () => {
  return __dirname + "/../testnet/untyped.plutus";
};

export const getScriptAddress = () => {
  // const scriptWalletName = "untyped";
  const scriptWalletName = "ticTacToeScript";
  cardanoCli.paymentAddressBuild(
    scriptWalletName,
    new PaymentAddressBuildOptions(PaymentComponent.scriptFile(getScriptFile()))
  );
  return cardanoCli.scriptWallet(scriptWalletName).paymentAddr;
};

export const getUntypedScriptAddress = () => {
  const scriptWalletName = "untyped";
  cardanoCli.paymentAddressBuild(
    scriptWalletName,
    new PaymentAddressBuildOptions(PaymentComponent.scriptFile(getUntypedScriptFile()))
  );
  return cardanoCli.scriptWallet(scriptWalletName).paymentAddr;
};

export const getUnitPlutusDataFile = () => {
  return __dirname + "/../testnet/unit.json";
};
