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
  