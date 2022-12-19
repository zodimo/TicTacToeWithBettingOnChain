import CardanocliJs from "cardanocli-js";
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

const cardanocliJs = new CardanocliJs({
  network: "testnet-magic 2",
  era: "babbage",
  dir: dir,
  shelleyGenesisPath: shelleyPath,
  socketPath: path.join(
    os.homedir(),
    "CardanoProjects",
    "testnet",
    "node.socket"
  ),
});

console.log(cardanocliJs.queryTip());
