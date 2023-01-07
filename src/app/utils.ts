import { Wallet } from "../cardano-cli/wallet.js";

export const printWallet = (account: string, wallet: Wallet) => {
  console.log("#################################");
  console.log(`###### ${account} Wallet ##########`);
  console.log("#################################");
  console.log(wallet);
};
