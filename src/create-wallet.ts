import {
  PaymentAddressBuildOptions,
  PaymentVerification,
  StakingVerification,
} from "./cardano-cli/address.js";
import { cardanoCli } from "./previewCardanoCliJs.js";
export const createWallet = (account: string) => {
  const paymentAddressKeys = cardanoCli.paymentAddressKeyGen(account);
  const stakeAddressKeys = cardanoCli.stakeAddressKeyGen(account);
  cardanoCli.paymentAddressBuild(
    account,
    new PaymentAddressBuildOptions(
      PaymentVerification.keyFile(paymentAddressKeys.verificationKeyFile),
      StakingVerification.keyFile(stakeAddressKeys.verificationKeyFile)
    )
  );
  return cardanoCli.wallet(account);
};
