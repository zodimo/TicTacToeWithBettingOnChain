import { CommandParameter } from "../../command-parameter.js";

export class OutputAs extends CommandParameter {
  private constructor(private paramKey: string, private paramValue: string) {
    super();
  }

  static outFile(value: string): OutputAs {
    // --out-file FILE
    const param = "out-file";
    return new OutputAs(param, value);
  }

  static calcylatePlutusScriptCost(value: string): OutputAs {
    // --calculate-plutus-script-cost FILE
    const param = "calculate-plutus-script-cost";
    return new OutputAs(param, value);
  }

  asParameter(): string {
    return `--${this.paramKey} ${this.paramValue}`;
  }
}
