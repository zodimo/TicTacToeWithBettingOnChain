import { PaymentAddressBuildOptions } from "./cardano-cli/address-build-options.js";
import { PaymentComponent, StakeComponent } from "./cardano-cli/command/address/build.js";
import { cardanoCli } from "./previewCardanoCliJs.js";
export const createWallet = (account: string) => {
  const paymentAddressKeys = cardanoCli.paymentAddressKeyGen(account);
  const stakeAddressKeys = cardanoCli.stakeAddressKeyGen(account);
  cardanoCli.paymentAddressBuild(
    account,
    new PaymentAddressBuildOptions(
      PaymentComponent.verificationKeyFile(paymentAddressKeys.verificationKeyFile),
      StakeComponent.verificationKeyFile(stakeAddressKeys.verificationKeyFile)
    )
  );
  return cardanoCli.wallet(account);
};
