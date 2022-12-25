import { CardanoCli, CardanoCliOptions } from "./cardano-cli/cardano-cli.js";
import { Network } from "./cardano-cli/network.js";

import os from "os";
import path from "path";
import { Era } from "./cardano-cli/era.js";

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
const era = Era.babbage();

const cardanoCliOptions = new CardanoCliOptions(shelleyPath, dir, era, network);
cardanoCliOptions.debug = true;

export const cardanoCli = new CardanoCli(cardanoCliOptions);
