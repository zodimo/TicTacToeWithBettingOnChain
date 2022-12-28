import { CommandParameter } from "../command-parameter.js";
import { Command } from "../command.js";

// Usage: cardano-cli transaction txid (--tx-body-file FILE | --tx-file FILE)

export class TxIdTx extends CommandParameter {
  private constructor(private paramKey: string, private paramValue: string) {
    super();
  }

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

export class TxId extends Command {
  private txIdTx?: TxIdTx;
  constructor(private commandPrefix: string) {
    super();
  }

  withTx(tx: TxIdTx): TxId {
    this.txIdTx = tx;
    return this;
  }

  getCommand(): string {
    let output: string[] = [this.commandPrefix, "txid"];
    if (this.txIdTx) {
      output.push(this.txIdTx.asParameter());
    }
    return output.join(" ");
  }
}
