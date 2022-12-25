import { Builder } from "./cardano-cli-builder/builder.js";
import { CardanoCliBin } from "./cardano-cli-builder/cardano-cli-bin.js";
import { cardanoCli } from "./new-cardano-cli.js";

const builder = new Builder(new CardanoCliBin("cardano-cli"));

const jacoWallet = cardanoCli.wallet("jaco");

builder.address((address) => {
  address.info((info) => {
    info.address(jacoWallet.paymentAddr);
    return info;
  });
  return address;
});

console.log(builder.toString());
console.log(builder.runCommand());
