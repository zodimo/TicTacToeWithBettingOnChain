import { CommandParameter } from "../command-parameter.js";
import { Command } from "../command.js";
import { OutFile } from "../shared/out-file.js";

/*
Usage: cardano-cli address key-hash 
            ( --payment-verification-key STRING
            | --payment-verification-key-file FILE
            )
            [--out-file FILE]
*/
export class PaymentVerificationKey extends CommandParameter {
  private constructor(private paramKey: string, private paramValue: string) {
    super();
  }

  static value(value: string): PaymentVerificationKey {
    //--payment-verification-key STRING
    const param = "payment-verification-key";
    return new PaymentVerificationKey(param, value);
  }
  static file(value: string): PaymentVerificationKey {
    // --payment-verification-key-file FILE
    const param = "payment-verification-key-file";
    return new PaymentVerificationKey(param, value);
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}

export class KeyHash extends Command {
  private paymentVerificationKey?: PaymentVerificationKey;
  private outFile?: OutFile;

  constructor(private commandPrefix: string) {
    super();
  }

  withPaymentVerificationKey(
    paymentVerificationKey: PaymentVerificationKey
  ): KeyHash {
    this.paymentVerificationKey = paymentVerificationKey;
    return this;
  }

  withOutFile(outFile: OutFile): KeyHash {
    this.outFile = outFile;
    return this;
  }

  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "key-hash"];
    if (this.paymentVerificationKey) {
      ouput.push(this.paymentVerificationKey.asParameter());
    }
    if (this.outFile) {
      ouput.push(this.outFile.asParameter());
    }
    return ouput.join(" ");
  }
}
