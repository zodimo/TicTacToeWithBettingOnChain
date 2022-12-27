import { Builder } from "../../cardano-cli-builder/builder.js";
import { CardanoCliBin } from "../../cardano-cli-builder/cardano-cli-bin.js";
import { cardanoCli } from "../../previewCardanoCliJs.js";

const builder = new Builder(new CardanoCliBin("cardano-cli"));

const jacoWallet = cardanoCli.wallet("jaco");

builder.transaction((transactionBuilder) => {
  transactionBuilder.buildRaw((buildRawBuilder) => {
    return buildRawBuilder;
  });
  return transactionBuilder;
});

console.log(builder.toString());
console.log(builder.runCommand());
