import { CommandParameter } from "../command-parameter.js";

export class StakePoolComponent extends CommandParameter {
  private constructor(private paramKey: string, private paramValue: string) {
    super();
  }

  static verificationKeyValue(value: string): StakePoolComponent {
    //--stake-pool-verification-key STRING
    const param = "stake-pool-verification-key";
    return new StakePoolComponent(param, value);
  }
  static coldVerificationKeyFile(value: string): StakePoolComponent {
    //--cold-verification-key-file FILE
    const param = "cold-verification-key-file";
    return new StakePoolComponent(param, value);
  }
  static poolId(value: string): StakePoolComponent {
    //--stake-pool-id STAKE-POOL-ID
    const param = "stake-pool-id";
    return new StakePoolComponent(param, value);
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}
