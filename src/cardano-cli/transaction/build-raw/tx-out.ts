export class TxOutAsset {
  constructor(public readonly name: string, public readonly amount: number) {}
}

export class TxOut {
  constructor(
    private readonly address: string,
    public valueInLovelace: number,
    private readonly assets: TxOutAsset[] | null = null
  ) {
    //--tx-out ADDRESS VALUE
  }

  asParameter(): string {
    let output = `--tx-out ${this.address}+${this.valueInLovelace}`;
    if (this.assets) {
      let assetOutput = "";
      this.assets.forEach((asset: TxOutAsset) => {
        assetOutput += `+${asset.amount} ${asset.name}`;
      });
      // slice(1) remove the leading +
      output += `+"${assetOutput.slice(1)}"`;
    }
    return output;
  }
}

export class TxOutDatum {
  private constructor(
    private paramKey: string,
    private paramValue: string,
    private isQuoted: boolean = false
  ) {}

  static hash(value: string): TxOutDatum {
    //--tx-out-datum-hash HASH
    const param = "tx-out-datum-hash";
    return new TxOutDatum(param, value);
  }

  static hashCborFile(value: string): TxOutDatum {
    //--tx-out-datum-hash-cbor-file CBOR FILE
    const param = "tx-out-datum-hash-cbor-file";
    return new TxOutDatum(param, value);
  }

  static hashFile(value: string): TxOutDatum {
    //--tx-out-datum-hash-file JSON FILE
    const param = "tx-out-datum-hash-file";
    return new TxOutDatum(param, value);
  }

  static hashValue(value: string): TxOutDatum {
    //--tx-out-datum-hash-value JSON VALUE
    const param = "tx-out-datum-hash-value";
    return new TxOutDatum(param, value);
  }

  static embedCborFile(value: string): TxOutDatum {
    //--tx-out-datum-embed-cbor-file CBOR FILE
    const param = "tx-out-datum-embed-cbor-file";
    return new TxOutDatum(param, value);
  }

  static embedFile(value: string): TxOutDatum {
    //--tx-out-datum-embed-file JSON FILE
    const param = "tx-out-datum-embed-file";
    return new TxOutDatum(param, value);
  }

  static embedValue(value: string): TxOutDatum {
    //--tx-out-datum-embed-value JSON VALUE
    const param = "tx-out-datum-embed-value";
    return new TxOutDatum(param, value, true);
  }

  static inlineCborFile(value: string): TxOutDatum {
    //--tx-out-inline-datum-cbor-file CBOR FILE
    const param = "tx-out-inline-datum-cbor-file";
    return new TxOutDatum(param, value);
  }

  static inlineFile(value: string): TxOutDatum {
    //--tx-out-inline-datum-file JSON FILE
    const param = "tx-out-inline-datum-file";
    return new TxOutDatum(param, value);
  }

  static inlineValue(value: string): TxOutDatum {
    //--tx-out-inline-datum-value JSON VALUE
    const param = "tx-out-inline-datum-value";
    return new TxOutDatum(param, value, true);
  }

  asParameter(): string {
    if (this.isQuoted) {
      return `--${this.paramKey} '${this.paramValue}'`;
    } else {
      return `--${this.paramKey} ${this.paramValue}`;
    }
  }
}

export class TxOutParameter {
  constructor(
    private txOut: TxOut,
    private datum: TxOutDatum | null = null,
    private referenceScriptFile: string | null = null
  ) {}

  setLovelaveValue(value: number) {
    this.txOut.valueInLovelace = value;
  }

  asParameter(): string {
    let output = this.txOut.asParameter();
    if (this.datum) {
      output = `${output} ${this.datum.asParameter()}`;
    }
    if (this.referenceScriptFile) {
      output = `${output} --tx-out-reference-script-file ${this.referenceScriptFile}`;
    }
    return output;
  }
}
