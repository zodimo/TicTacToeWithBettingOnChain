//TX-IN
export class SpendingReferenceTxInDatum {
  private constructor(private paramKey: string, private paramValue: string) {}

  static cborFile(value: string): SpendingReferenceTxInDatum {
    //  --spending-reference-tx-in-datum-cbor-file CBOR FILE
    const param = "spending-reference-tx-in-datum-cbor-file";
    return new SpendingReferenceTxInDatum(param, value);
  }
  static file(value: string): SpendingReferenceTxInDatum {
    // --spending-reference-tx-in-datum-file JSON FILE
    const param = "spending-reference-tx-in-datum-file";
    return new SpendingReferenceTxInDatum(param, value);
  }
  static value(value: string): SpendingReferenceTxInDatum {
    // --spending-reference-tx-in-datum-value JSON VALUE
    const param = "spending-reference-tx-in-datum-value";
    return new SpendingReferenceTxInDatum(param, value);
  }
  static inlineDatumIsPresent(): SpendingReferenceTxInDatum {
    // --spending-reference-tx-in-inline-datum-present
    const param = "spending-reference-tx-in-inline-datum-present";
    return new SpendingReferenceTxInDatum(param, "");
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}

export class SpendingReferenceTxInRedeemer {
  private constructor(private paramKey: string, private paramValue: string) {}

  static cborFile(value: string): SpendingReferenceTxInRedeemer {
    // --spending-reference-tx-in-redeemer-cbor-file CBOR FILE
    const param = "spending-reference-tx-in-redeemer-cbor-file";
    return new SpendingReferenceTxInRedeemer(param, value);
  }
  static file(value: string): SpendingReferenceTxInRedeemer {
    // --spending-reference-tx-in-redeemer-file JSON FILE
    const param = "spending-reference-tx-in-redeemer-file";
    return new SpendingReferenceTxInRedeemer(param, value);
  }
  static value(value: string): SpendingReferenceTxInRedeemer {
    // --spending-reference-tx-in-redeemer-value JSON VALUE
    const param = "spending-reference-tx-in-redeemer-value";
    return new SpendingReferenceTxInRedeemer(param, value);
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}

export class GroupX {
  private constructor(private paramKey: string, private paramValue: string) {}

  static spendingReferenceTxInExecutionUnits(
    timeUnits: number,
    spaceUnits: number
  ): GroupX {
    // --spending-reference-tx-in-execution-units (INT, INT)
    const param = "spending-reference-tx-in-execution-units";
    return new GroupX(param, `(${timeUnits}, ${spaceUnits})`);
  }

  static simpleScriptTxInReference(value: string): GroupX {
    // --simple-script-tx-in-reference TX-IN
    const param = "simple-script-tx-in-reference";
    return new GroupX(param, value);
  }

  static txInScriptFile(value: string): GroupX {
    //--tx-in-script-file FILE
    const param = "tx-in-script-file";
    return new GroupX(param, value);
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}

export class TxInDatum {
  private constructor(private paramKey: string, private paramValue: string) {}

  static cborFile(value: string): TxInDatum {
    // --tx-in-datum-cbor-file CBOR FILE
    const param = "tx-in-datum-cbor-file";
    return new TxInDatum(param, value);
  }

  static file(value: string): TxInDatum {
    //  --tx-in-datum-file JSON FILE
    const param = "tx-in-datum-file";
    return new TxInDatum(param, value);
  }

  static value(value: string): TxInDatum {
    // --tx-in-datum-value JSON VALUE
    const param = "tx-in-datum-value";
    return new TxInDatum(param, value);
  }

  static inlineDatumInPresent(): TxInDatum {
    // --tx-in-inline-datum-present
    const param = "tx-in-inline-datum-present";
    return new TxInDatum(param, "");
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}
export class TxInRedeemer {
  private constructor(private paramKey: string, private paramValue: string) {}

  static cborFile(value: string): TxInRedeemer {
    //--tx-in-redeemer-cbor-file CBOR FILE
    const param = "tx-out-datum-hash";
    return new TxInRedeemer(param, value);
  }
  static file(value: string): TxInRedeemer {
    //--tx-in-redeemer-file JSON FILE
    const param = "tx-out-datum-hash";
    return new TxInRedeemer(param, value);
  }
  static value(value: string): TxInRedeemer {
    //--tx-in-redeemer-value JSON VALUE
    const param = "tx-out-datum-hash";
    return new TxInRedeemer(param, value);
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}

export class TxInExecutionUnits {
  constructor(private timeUnits: number, private spaceUnits: number) {}
  asParameter(): string {
    return `--tx-in-execution-units (${this.timeUnits}, ${this.spaceUnits})`;
  }
}

export class TxInSpending {
  constructor(
    private spendingTxInReference: string,
    private isSpendingPlutusScriptV2: boolean,
    private spendingReferenceTxInDatum: SpendingReferenceTxInDatum,
    private spendingReferenceTxInRedeemer: SpendingReferenceTxInRedeemer,
    private groupX: GroupX,
    private txInDatum: TxInDatum,
    private txInRedeemer: TxInRedeemer,
    private txInExecutionUnits: TxInExecutionUnits
  ) {}

  asParameter(): string {
    let output = `--spending-tx-in-reference ${this.spendingTxInReference}`;

    if (this.isSpendingPlutusScriptV2) {
      output += " --spending-plutus-script-v2";
    }

    output += this.spendingReferenceTxInDatum.asParameter();
    output += this.spendingReferenceTxInRedeemer.asParameter();
    output += this.groupX.asParameter();
    output += this.txInDatum.asParameter();
    output += this.txInRedeemer.asParameter();
    output += this.txInExecutionUnits.asParameter();

    return output;
  }
}

export class TxInParameter {
  constructor(
    public txIn: string,
    public txInSpending: TxInSpending | null = null
  ) {}
  asParameter(): string {
    let output = `--tx-in ${this.txIn}`;
    if (this.txInSpending) {
      output += ` ${this.txInSpending.asParameter()}`;
    }
    return output;
  }
}

//TX_OUT
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

//TRANSACTION

export class TransactionBuildRawOptions {
  constructor(
    public txIn: TxInParameter[],
    public txOut: TxOutParameter[],
    public fee: number = 0
  ) {}
}

export class TransactionCalculateMinFeeOptions {
  constructor(
    public readonly txBodyFile: string,
    public readonly txInCount: number,
    public readonly txOutCount: number,
    public readonly witnessCount: number,
    public readonly byronWitnessCount: number = 0
  ) {}
}

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

export class TransactionSubmitOptions {
  constructor(public readonly txFile: string) {}
}

export class TxIdTx {
  private constructor(private paramKey: string, private paramValue: string) {}

  static bodyFile(value: string): TxIdTx {
    //--tx-body-file FILE
    const param = "tx-body-file";
    return new TxIdTx(param, value);
  }
  static file(value: string): TxIdTx {
    //--tx-file FILE
    const param = "tx-file";
    return new TxIdTx(param, value);
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}

export class TxIdOptions {
  constructor(public readonly tx: TxIdTx) {}
}
