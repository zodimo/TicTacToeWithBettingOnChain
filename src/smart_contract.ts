import cardanocliJs from "./previewCardanoCliJs.js";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// get script address
export const scriptPath = __dirname + "/../testnet/ticTacToe.plutus";
const scriptRaw = fs.readFileSync(scriptPath).toString();
export const script=JSON.parse(scriptRaw);
export const policyId=cardanocliJs.transactionPolicyid(script);
