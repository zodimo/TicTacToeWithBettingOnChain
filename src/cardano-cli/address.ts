export class PaymentVerification {
  private constructor(private paramKey: string, private paramValue: string) {}

  static key(value: string): PaymentVerification {
    // --payment-verification-key STRING
    const param = "payment-verification-key";
    return new PaymentVerification(param, value);
  }
  static keyFile(value: string): PaymentVerification {
    // --payment-verification-key-file FILE
    const param = "payment-verification-key-file";
    return new PaymentVerification(param, value);
  }
  static scriptFile(value: string): PaymentVerification {
    // --payment-script-file FILE
    const param = "payment-script-file";
    return new PaymentVerification(param, value);
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}

export class StakingVerification {
  private constructor(private paramKey: string, private paramValue: string) {}

  static key(value: string): StakingVerification {
    //--stake-verification-key STRING
    const param = "stake-verification-key";
    return new StakingVerification(param, value);
  }
  static keyFile(value: string): StakingVerification {
    // --stake-verification-key-file FILE
    const param = "stake-verification-key-file";
    return new StakingVerification(param, value);
  }
  static scriptFile(value: string): StakingVerification {
    // --stake-script-file FILE
    const param = "stake-script-file";
    return new StakingVerification(param, value);
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}

export class PaymentAddressBuildOptions {
  constructor(
    public readonly paymentVerification: PaymentVerification,
    public readonly stakingVerification?: StakingVerification
  ) {}
}
