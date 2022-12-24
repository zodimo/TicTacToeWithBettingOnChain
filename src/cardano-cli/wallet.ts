export interface AddressKeysInterface {
  readonly verificationKeyFile: string;
  readonly signingKeyFile: string;
}

export class AddressKeys implements AddressKeys {
  constructor(
    public readonly verificationKeyFile: string,
    public readonly signingKeyFile: string
  ) {}
}

export class WalletKeys {
  constructor(public readonly payment: AddressKeys) {}
}
export class Wallet {
  constructor(
    public readonly name: string,
    public readonly paymentAddr: string,
    public readonly stakingAddr: string | null,
    public readonly keys: WalletKeys
  ) {}
}

export class ScriptWallet {
  constructor(
    public readonly name: string,
    public readonly paymentAddr: string
  ) {}
}
