import cardanocliJs from "./previewCardanoCliJs.js";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";



export const getScriptAddress = () => {

  const __filename = fileURLToPath(import.meta.url);
  const __dirname = path.dirname(__filename);
  
  // get script address
  const scriptPath = __dirname + "/../testnet/ticTacToe.plutus";
  const scriptRaw = fs.readFileSync(scriptPath).toString();
  const script = JSON.parse(scriptRaw);

  const scriptWalletName = "ticTacToeScript";
  cardanocliJs.addressBuild(scriptWalletName, {
    paymentScript: script,
  });
  return cardanocliJs.wallet(scriptWalletName).paymentAddr;
};

// console.log(getScriptAddress());