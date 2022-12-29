import { CommandParameter } from "../../command-parameter.js";

export class TxToSign extends CommandParameter{
    private constructor(private paramKey: string, private paramValue: string) {
      super();
    }
  
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
  