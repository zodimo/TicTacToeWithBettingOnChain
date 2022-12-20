
import cardanocliJs from "./previewCardanoCliJs.js";

export const createWallet = (account:string) => {
    const payment = cardanocliJs.addressKeyGen(account);
    const stake = cardanocliJs.stakeAddressKeyGen(account);
    cardanocliJs.stakeAddressBuild(account);
    cardanocliJs.addressBuild(account, {
      paymentVkey: payment.vkey,
      stakeVkey: stake.vkey,
    });
    return cardanocliJs.wallet(account);
  };
