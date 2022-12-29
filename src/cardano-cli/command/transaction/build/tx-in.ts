import { UtxoId } from "../../../utxo-id.js";
import { CommandParameter } from "../../command-parameter.js";

export class TxIn {
  constructor(private txIn: UtxoId) {}
  asParameter(): string {
    return `--tx-in ${this.txIn}`;
  }
}

export class SpendingTxInReference extends CommandParameter {
  constructor(private txIn: UtxoId) {
    super();
  }
  asParameter(): string {
    return `--spending-tx-in-reference ${this.txIn}`;
  }
}
export class SpendingPlutusScriptV2 extends CommandParameter {
  asParameter(): string {
    return `--spending-plutus-script-v2`;
  }
}

export class SpendingReferenceTxInDatum extends CommandParameter {
  private constructor(private paramKey: string, private paramValue: string) {
    super();
  }

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

export class TxInSpending extends CommandParameter {
  private spendingTxInReference?: SpendingTxInReference;
  private spendingPlutusScriptV2?: SpendingPlutusScriptV2;
  private spendingReferenceTxInDatum?: SpendingReferenceTxInDatum;
  private spendingReferenceTxInRedeemer?: SpendingReferenceTxInRedeemer;

  withSpendingTxInReference(
    spendingTxInReference: SpendingTxInReference
  ): TxInSpending {
    this.spendingTxInReference = spendingTxInReference;
    return this;
  }

  withSpendingPlutusScriptV2(): TxInSpending {
    this.spendingPlutusScriptV2 = new SpendingPlutusScriptV2();
    return this;
  }

  withSpendingReferenceTxInDatum(
    spendingReferenceTxInDatum: SpendingReferenceTxInDatum
  ): TxInSpending {
    this.spendingReferenceTxInDatum = spendingReferenceTxInDatum;
    return this;
  }

  withSpendingReferenceTxInRedeemer(
    spendingReferenceTxInRedeemer: SpendingReferenceTxInRedeemer
  ): TxInSpending {
    this.spendingReferenceTxInRedeemer = spendingReferenceTxInRedeemer;
    return this;
  }

  asParameter(): string {
    const outputParams: string[] = [];
    if (this.spendingTxInReference) {
      outputParams.push(this.spendingTxInReference.asParameter());
    }

    if (this.spendingPlutusScriptV2) {
      outputParams.push(this.spendingPlutusScriptV2.asParameter());
    }

    if (this.spendingReferenceTxInDatum) {
      outputParams.push(this.spendingReferenceTxInDatum.asParameter());
    }

    if (this.spendingReferenceTxInRedeemer) {
      outputParams.push(this.spendingReferenceTxInRedeemer.asParameter());
    }

    return outputParams.join(" ");
  }
}

export class SimpleScriptTxInReference extends CommandParameter {
  constructor(private txIn: UtxoId) {
    super();
  }
  asParameter(): string {
    throw `--simple-script-tx-in-reference ${this.txIn.toString()}`;
  }
}

export class TxInScriptFile extends CommandParameter {
  constructor(private filename: string) {
    super();
  }
  asParameter(): string {
    return `--tx-in-script-file ${this.filename}`;
  }
}

export class TxInAlternativeFactory {
  private constructor() {}
  static txInSpending(): TxInSpending {
    return new TxInSpending();
  }

  static simpleScriptTxInReference(txId: UtxoId): SimpleScriptTxInReference {
    return new SimpleScriptTxInReference(txId);
  }
  static txInScriptFile(filename: string): TxInScriptFile {
    return new TxInScriptFile(filename);
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

export class TxInScriptAdditional extends CommandParameter {
  private txInDatum?: TxInDatum;
  private txInRedeemer?: TxInRedeemer;
  private txInExecutionUnits?: TxInExecutionUnits;

  withTxInDatum(txInDatum: TxInDatum): TxInScriptAdditional {
    this.txInDatum = txInDatum;
    return this;
  }
  withTxInRedeemer(txInRedeemer: TxInRedeemer): TxInScriptAdditional {
    this.txInRedeemer = txInRedeemer;
    return this;
  }
  withTxInExecutionUnits(
    txInExecutionUnits: TxInExecutionUnits
  ): TxInScriptAdditional {
    this.txInExecutionUnits = txInExecutionUnits;
    return this;
  }

  asParameter(): string {
    const output: string[] = [];
    if (this.txInDatum) {
      output.push(this.txInDatum.asParameter());
    }
    if (this.txInRedeemer) {
      output.push(this.txInRedeemer.asParameter());
    }
    if (this.txInExecutionUnits) {
      output.push(this.txInExecutionUnits.asParameter());
    }
    return output.join(" ");
  }
}

export class TxInAdditional extends CommandParameter {
  constructor(
    private txInAlternative:
      | TxInSpending
      | SimpleScriptTxInReference
      | TxInScriptFile,
    private txInScriptAdditional?: TxInScriptAdditional
  ) {
    super();
  }

  asParameter(): string {
    const output: string[] = [this.txInAlternative.asParameter()];
    return output.join(" ");
  }
}

export class TxInParameter extends CommandParameter {
  constructor(private txIn: TxIn, private txInAdditional?: TxInAdditional) {
    super();
  }
  asParameter(): string {
    const output: string[] = [this.txIn.asParameter()];
    if (this.txInAdditional) {
      output.push(this.txInAdditional.asParameter());
    }
    return output.join(" ");
  }
}
