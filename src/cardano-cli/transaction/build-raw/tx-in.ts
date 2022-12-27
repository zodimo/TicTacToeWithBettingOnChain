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