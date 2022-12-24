export class Network {
  private constructor(private paramKey: string, private paramValue: string) {}

  static mainnet(): Network {
    //--mainnet
    const param = "mainnet";
    return new Network(param, "");
  }

  static testnetMagic(value: number): Network {
    //--mainnet
    const param = "testnet-magic";
    return new Network(param, value.toString());
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}
