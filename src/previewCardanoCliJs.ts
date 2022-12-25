import { CardanoCli, CardanoCliOptions } from "./cardano-cli/cardano-cli.js";
import {Network} from "./cardano-cli/network.js"

import os from "os";
import path from "path";

const dir = path.join(os.homedir(), "CardanoProjects", "testnet");
const shelleyPath = path.join(
  os.homedir(),
  "CardanoProjects",
  "testnet",
  "config",
  "preview",
  "shelley-genesis.json"
);

const network = Network.testnetMagic(2);

const cardanoCliOptions= new CardanoCliOptions(shelleyPath, dir, "babbage",network);
cardanoCliOptions.debug=true;


export const cardanoCli = new CardanoCli(
  cardanoCliOptions
);