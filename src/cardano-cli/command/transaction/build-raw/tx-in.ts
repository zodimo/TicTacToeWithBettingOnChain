import { CommandParameter } from "../../command-parameter.js";

export class TxInParameter extends CommandParameter {
    constructor(private txIn: string) {
        super();
    }
    asParameter(): string {
      return `--tx-in ${this.txIn}`;
    }
  }