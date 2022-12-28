import { CommandParameter } from "../../command-parameter.js";

export class Filter extends CommandParameter {
  private constructor(private paramKey: string, private paramValue: string) {
    super();
  }

  static address(value: string): Filter {
    //--address ADDRESS
    const param = "address";
    return new Filter(param, value);
  }
  static txIn(value: string): Filter {
    //--tx-in TX-IN
    const param = "tx-in";
    return new Filter(param, value);
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}
