import { CommandParameter } from "../command-parameter.js";

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
