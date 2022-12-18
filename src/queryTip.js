const CardanocliJs = require("cardanocli-js");
const os = require("os");
const path = require("path");

const dir = path.join(os.homedir(), "CardanoProjects","testnet");
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
  socketPath: path.join(os.homedir(), "CardanoProjects","testnet", "node.socket"),
});

console.log(cardanocliJs.queryTip());
