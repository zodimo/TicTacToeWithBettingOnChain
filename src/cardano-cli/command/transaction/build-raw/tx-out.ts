import { CommandParameter } from "../../command-parameter.js";

export class TxOutAsset {
  constructor(public readonly name: string, public readonly amount: number) {}
}

/*
  --tx-out ADDRESS VALUE   The transaction output as ADDRESS VALUE where ADDRESS
                           is the Bech32-encoded address followed by the value
                           in the mult
*/
export class TxOut extends CommandParameter {
  constructor(
    private readonly address: string,
    public valueInLovelace: number,
    private readonly assets: TxOutAsset[] | null = null
  ) {
    super();
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

export class TxOutDatum extends CommandParameter {
  private constructor(
    private paramKey: string,
    private paramValue: string,
    private isQuoted: boolean = false
  ) {
    super();
  }

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

export class TxOutParameter extends CommandParameter {
  constructor(private txOut?: TxOut, private txOutDatum?: TxOutDatum) {
    super();
  }

  withTxOut(txOut: TxOut): TxOutParameter {
    this.txOut = txOut;
    return this;
  }
  withTxOutDatum(txOutDatum: TxOutDatum): TxOutParameter {
    this.txOutDatum = txOutDatum;
    return this;
  }

  setLovelaveValue(value: number) {
    if (!this.txOut) {
      throw new Error("txOut is not set.");
    }
    this.txOut.valueInLovelace = value;
  }

  asParameter(): string {
    const output: string[] = [];
    if (this.txOut) {
      output.push(this.txOut.asParameter());
    }

    if (this.txOutDatum) {
      output.push(this.txOutDatum.asParameter());
    }
    return output.join(" ");
  }
}
