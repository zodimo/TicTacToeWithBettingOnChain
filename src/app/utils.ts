import { UtxoStack } from "../cardano-cli/utxo.js";
import { Wallet } from "../cardano-cli/wallet.js";

export const printWallet = (account: string, wallet: Wallet) => {
  console.log("#################################");
  console.log(`       ${account} Wallet `);
  console.log("#################################");
  console.log(wallet);
};

export const printWalletUtxoStack = (account: string, utxoStack: UtxoStack) => {
  console.log("##################################################");
  console.log(`           ${account} Wallet Utxo STACK`);
  console.log("##################################################");
  console.log(JSON.stringify(utxoStack, null, 2));
};
