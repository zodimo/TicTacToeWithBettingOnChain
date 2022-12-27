export class TxToSign {
  private constructor(private paramKey: string, private paramValue: string) {}

  static txBodyFile(value: string): TxToSign {
    //--tx-body-file FILE
    const param = "tx-body-file";
    return new TxToSign(param, value);
  }
  static txFile(value: string): TxToSign {
    //--tx-file FILE
    const param = "tx-file";
    return new TxToSign(param, value);
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}

export class SigningKeyFiles {
  constructor(public readonly signingKeyFiles: string[]) {}

  asParameter(): string {
    let output: string = "";
    this.signingKeyFiles.forEach((signingKeyFile) => {
      output += ` --signing-key-file ${signingKeyFile}`;
    });
    return output.slice(1);
  }
}

export class ByronAddress {
  constructor(private readonly address?: string) {}
  asParameter(): string {
    if (this.address) {
      return `--address ${this.address}`;
    }
    return "";
  }
}

export class TransactionSignOptions {
  public readonly address: ByronAddress;
  constructor(
    public readonly txToSign: TxToSign,
    public readonly signingKeyFiles: SigningKeyFiles,
    address?: ByronAddress
  ) {
    if (address) {
      this.address = address;
    } else {
      this.address = new ByronAddress();
    }
  }
}
