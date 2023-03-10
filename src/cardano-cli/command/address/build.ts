import { config } from "../../../cardano-cli-config.js";
import { CommandParameter } from "../command-parameter.js";
import { Command } from "../command.js";
import { Network } from "../network.js";
import { OutFile } from "../shared/out-file.js";

/*
Usage: cardano-cli address build 
            ( --payment-verification-key STRING
            | --payment-verification-key-file FILE
            | --payment-script-file FILE
            )
            [ --stake-verification-key STRING
            | --stake-verification-key-file FILE
            | --stake-script-file FILE
            ]
            (--mainnet | --testnet-magic NATURAL)
            [--out-file FILE]
*/
export class PaymentComponent extends CommandParameter {
  private constructor(private paramKey: string, private paramValue: string) {
    super();
  }

  static verificationKeyValue(value: string): PaymentComponent {
    //--payment-verification-key STRING
    const param = "payment-verification-key";
    return new PaymentComponent(param, value);
  }
  static verificationKeyFile(value: string): PaymentComponent {
    //--payment-verification-key-file FILE
    const param = "payment-verification-key-file";
    return new PaymentComponent(param, value);
  }
  static scriptFile(value: string): PaymentComponent {
    //--payment-script-file FILE
    const param = "payment-script-file";
    return new PaymentComponent(param, value);
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}
export class StakeComponent extends CommandParameter {
  private constructor(private paramKey: string, private paramValue: string) {
    super();
  }

  static verificationKeyValue(value: string): StakeComponent {
    //--stake-verification-key STRING
    const param = "stake-verification-key";
    return new StakeComponent(param, value);
  }
  static verificationKeyFile(value: string): StakeComponent {
    //--stake-verification-key-file FILE
    const param = "stake-verification-key-file";
    return new StakeComponent(param, value);
  }
  static scriptFile(value: string): StakeComponent {
    //--stake-script-file FILE
    const param = "stake-script-file";
    return new StakeComponent(param, value);
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}

export class Build extends Command {
  private paymentComponent?: PaymentComponent;
  private stakeComponent?: StakeComponent;
  private network?: Network;
  private outFile?: OutFile;
  constructor(private commandPrefix: string) {
    super();
    this.network = config.getNetwork();
  }

  withPaymentComponent(paymentComponent: PaymentComponent): Build {
    this.paymentComponent = paymentComponent;
    return this;
  }
  withStakeComponent(stakeComponent: StakeComponent): Build {
    this.stakeComponent = stakeComponent;
    return this;
  }

  withNetwork(network: Network): Build {
    this.network = network;
    return this;
  }

  withOutFile(outFile: OutFile): Build {
    this.outFile = outFile;
    return this;
  }

  getCommand(): string {
    let ouput: string[] = [this.commandPrefix, "build"];

    if (this.paymentComponent) {
      ouput.push(this.paymentComponent.asParameter());
    }
    if (this.stakeComponent) {
      ouput.push(this.stakeComponent.asParameter());
    }
    if (this.network) {
      ouput.push(this.network.asParameter());
    }
    if (this.outFile) {
      ouput.push(this.outFile.asParameter());
    }
    return ouput.join(" ");
  }
}
